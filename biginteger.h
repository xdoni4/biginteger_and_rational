#include <iostream>
#include <vector>
#include <string>

static const std::string DIGITS = "0123456789";
static const int RADIX = 1 << 15;
std::string& reverse(std::string& s);

class BigInteger;
class Rational;

BigInteger operator*(const BigInteger &a, const BigInteger &b);
std::ostream& operator<<(std::ostream &out, const BigInteger &a);
std::ostream& operator<<(std::ostream &out, const Rational &a);
bool operator==(const BigInteger &a, const BigInteger &b);
bool operator>(const BigInteger &a, const BigInteger &b);
bool operator<(const BigInteger &a, const BigInteger &b);


class BigInteger {
public:  
    BigInteger() {
        sign = 0;
        body.resize(1);
        body[0] = 0;
    }

    BigInteger(int init) {
        if (init == 0) {
            sign = 0;
            body.resize(1);
            body[0] = 0;
        }
        sign = -(init >> 31);
        init = init < 0 ? -init : init;
        size_t index = 0;
        for (; init > 0; init /= RADIX) {
            int rem = init % RADIX;
            body.push_back(rem);
            index++;
        }
    } 

    BigInteger(std::string s) {
        bool keepsign = s[0] == '-' ? 1 : 0;
        if (keepsign) {
            s.erase(0, 1);
        }
        sign = keepsign;
        BigInteger tmp = 1;
        for (size_t i = 0; i < s.size(); i++) {
            (*this) += tmp * (s[s.size() - i - 1] - '0'); 
            tmp *= 10;
        }
        sign = keepsign;
    }

    BigInteger(const BigInteger &b) {
        sign = b.sign;
        body = b.body;
    }

    ~BigInteger() {}

    BigInteger& operator=(const BigInteger &b) {
        sign = b.sign;
        body = b.body;
        return (*this);
    } 

    BigInteger& operator+=(const BigInteger &b) {
        if (sign ^ b.sign) { 
            BigInteger a_abs = (*this); a_abs.abs();
            BigInteger b_abs = b; b_abs.abs();
            if ((a_abs == b_abs)) {
                (*this) = 0;
                return (*this);
            }
            if ((a_abs > b_abs)) {
                int keepsign = sign;
                a_abs -= b_abs;
                (*this) = a_abs;
                sign = keepsign;
                return (*this);
            }
            int keepsign = b.sign;
            b_abs -= a_abs;
            (*this) = b_abs;
            sign = keepsign;
            if (this->is_zero()) sign = 0;
            return (*this);
        }
        BigInteger bcp = b;
        if (body.size() < bcp.body.size()) {
            body.resize(bcp.body.size());
        }
        int carry = 0;
        for (size_t i = 0; i < bcp.body.size(); i++) {
            int sum = body[i] + bcp.body[i] + carry;
            body[i] = sum % RADIX;
            carry = sum / RADIX;
        }
        if (carry) {
            body.resize(body.size() + 1);
            int index = 0;
            while (carry) {
                int sum = body[bcp.body.size() + index] + carry;
                body[bcp.body.size() + index] = sum % RADIX;
                carry = sum / RADIX;
                index++;
            }
            int diff = 0;
            while (body[body.size() - diff - 1] == 0 && body.size() - diff > 1) {
                diff++;
            }
            body.resize(body.size() - diff);
        }
        return (*this);
    }

    BigInteger& operator-=(const BigInteger &b) {
        if ((sign ^ b.sign) == 0) { 
            BigInteger a_abs = (*this); a_abs.abs();
            BigInteger b_abs = b; b_abs.abs(); 
            if ((a_abs == b_abs)) {
                (*this) = 0;
                return (*this);
            }
            if ((a_abs > b_abs)) {
                int keepsign = sign;
                int carry = 0;
                for (size_t i = 0; i < b_abs.body.size(); i++) {
                    a_abs.body[i] -= carry;
                    carry = 0;
                    a_abs.body[i] -= b_abs.body[i];
                    if (a_abs.body[i] < 0) {
                        a_abs.body[i] += RADIX;
                        carry++;
                    }
                }
                int index = b_abs.body.size();
                while (carry > 0 ) {
                    a_abs.body[index] -= carry;
                    carry = 0;
                    if (a_abs.body[index] < 0) {
                        a_abs.body[index] += RADIX;
                        carry++;
                    }
                    index++;
                }
                int diff = 0;
                while (a_abs.body[a_abs.body.size() - diff - 1] == 0 && a_abs.body.size() - diff > 1) {
                    diff++;
                }
                a_abs.body.resize(a_abs.body.size() - diff);
                (*this) = a_abs;
                sign = keepsign;
                return (*this);
            }
            int keepsign = sign;
            int carry = 0;
            for (size_t i = 0; i < a_abs.body.size(); i++) {
                b_abs.body[i] -= carry;
                carry = 0;
                b_abs.body[i] -= a_abs.body[i];
                if (b_abs.body[i] < 0) {
                    b_abs.body[i] += RADIX;
                    carry++;
                }
            }
            int index = a_abs.body.size();
            while (carry > 0) {
                b_abs.body[index] -= carry;
                carry = 0;
                if (b_abs.body[index] < 0) {
                    b_abs.body[index] += RADIX;
                    carry++;
                }
                index++;
            }
            int diff = 0;
            while (b_abs.body[b_abs.body.size() - diff - 1] == 0 && b_abs.body.size() - diff > 1) {
                diff++;
            }    
            b_abs.body.resize(b_abs.body.size() - diff);
            (*this) = b_abs;
            sign = keepsign ^ 1;
            if (this->is_zero()) sign = 0;
            return (*this);
        }
        int keepsign = sign;
        BigInteger a_abs = (*this); a_abs.abs();
        BigInteger b_abs = b; b_abs.abs();
        a_abs += b_abs;
        (*this) = a_abs;
        sign = keepsign;
        return (*this);  
    }

    BigInteger& operator*=(const BigInteger &b) {
        if (b == 0 || this->is_zero()) {
            (*this) = 0;
            return (*this);
        }
        BigInteger res;  
        int keepsign = sign ^ b.sign;
        for (size_t i = 0; i < b.body.size(); i++) {
            BigInteger tmp((*this));
            tmp.mul_short(b.body[i]);
            tmp.shift_left(i);
            res += tmp; 
        }
        (*this) = res; 
        int diff = 0;
        while (body[body.size() - diff - 1] == 0 && body.size() - diff > 1) {
            diff++;
        }
        body.resize(body.size() - diff);
        sign = keepsign;
        return (*this);
    }

    BigInteger& operator/=(const BigInteger &divider) {
        BigInteger rem;
        (*this).div(divider, rem);
        return (*this);
    }

    BigInteger& operator%=(const BigInteger &divider) {
        BigInteger rem, divcp = divider;
        (*this).div(divcp, rem);
        (*this) = rem;
        return (*this);
    }

    BigInteger& operator++() {
        (*this) += 1;
        return (*this);
    }

    BigInteger& operator--() {
        (*this) -= 1;
        return (*this);
    }

    BigInteger operator++(int) {
        BigInteger tmp = (*this);
        (*this) += 1;
        return tmp;
    }

    BigInteger operator--(int) {
        BigInteger tmp = (*this);
        (*this) -= 1;
        return tmp;
    }

    explicit operator bool() {
        return this->is_zero() ? false : true; 
    }

    int BigIntegerNeg() {
        if (!this->is_zero()) sign ^= 1;
        return 0;
    }

    bool BigIntegerSign() {
        return sign;
    }

    int BigIntegerCompare(const BigInteger &b) const {
        if (this->is_zero() && b.is_zero()) return 0;
        if (sign ^ b.sign) {
            if (sign) {
                return -1;
            }
            else {
                return 1;
            }
        }
        if (sign) {
            if (body.size() > b.body.size()) {
                return -1;
            }
            else if (body.size() < b.body.size()) {
                return 1;
            }
            else {
                for (size_t i = body.size(); i > 0; i--) {
                    if (body[i - 1] > b.body[i - 1]){
                        return -1;
                    }
                    else if (body[i - 1] < b.body[i - 1]) {
                        return 1;
                    }
                }
                return 0;
            }
        }
        if (body.size() > b.body.size()) {
            return 1;
        }
        else if (body.size() < b.body.size()) {
            return -1;
        }
        else {
            for (size_t i = body.size(); i > 0; i--) {
                if (body[i - 1] > b.body[i - 1]) {
                    return 1;
                }
                else if (body[i - 1] < b.body[i - 1]) {
                    return -1;
                }
            }
            return 0;
        }
    }

    std::string toString() const { 
        if (this->is_zero()) {
            std::string out = "0";
            return out;
        }
        BigInteger copy = (*this);
        int a = 0; int &rem = a;
        std::string out;
        size_t index = 0;
        while (copy.body[0] > 0 || copy.body.size() > 1) {
            copy.div_short(10, rem);
            out.push_back(DIGITS[rem]);
            index++;
        }
        if (sign) out.push_back('-');
        reverse(out);
        return out;
    }
    int mul(int mult) {
        mul_short(mult);
        return 0;
    }

    int abs() {
        sign = 0;
        return 0;
    }

    bool is_zero() const {
        return body.size() == 1 && body[0] == 0;
    }

private:
    bool sign;
    std::vector <int> body;  
    
    int shift_left(size_t k) {
        if (k == 0) return 0;
        body.resize(body.size() + k);
        for (size_t i = 0; i < k; i++) {
            body[body.size() - k + i] = 0;
        }
        for (size_t i = body.size(); i > k; i--) {
            body[i - 1] = body[i - 1 - k];
            body[i - 1 - k] = 0;
        }
        return 0;
    }

    int search_mult(const BigInteger &divider) {
        int l = 0, r = RADIX;
        while (l + 1 < r) {
            int m = (l + r) / 2;
            BigInteger prod = divider; 
            prod.mul_short(m); 
            int code = prod.BigIntegerCompare((*this));
            if (code == -1) {
                l = m;
            }
            else if (code == 0) {
                return m;
            }
            else if (code == 1){
                r = m;
            }
        }
        BigInteger prod = divider; 
        prod.mul_short(r); 
        int ret = prod > (*this) ? l : r;
        return ret;
    }

    int mul_short(int mult) {
        if (this->is_zero()) (*this) = 0;
        sign ^= -(mult >> 31);
        mult = mult < 0 ? -mult : mult;
        int carry = 0;
        for (size_t i = 0; i < body.size(); i++) {
            int product = body[i] * mult + carry;
            body[i] = product % RADIX;
            carry = product / RADIX;
        }
        if (carry) {
            body.resize(body.size() + 1);
            body[body.size() - 1] = carry;
        }
        return 0;
    }

    int div_short(int div, int &rem) {
        if (this->is_zero()) (*this) = 0;
        sign ^= -(div >> 31);
        div = div < 0 ? -div : div;
        if (body.size() == 1) {
            rem = body[0] % div;
            body[0] /= div;
            return 0;
        }
        int curint = 0;
        for (size_t i = body.size(); i > 0; i--) {
            curint += body[i - 1];
            if (curint < div) {
                body[i - 1] = 0;
                curint *= RADIX;
                continue;
            }
            body[i - 1] = curint / div;
            curint %= div;
            curint *= RADIX;
        }
        curint /= RADIX;
        rem = curint;
        size_t ind = 0;
        while (body[body.size() - ind - 1] == 0 && body.size() - ind - 1 != 0) {
            ind++;
        }
        body.resize(body.size() - ind);
        return 0;
    }
    
    int div(const BigInteger &diver, BigInteger &rem) {
        if (this->is_zero()) (*this) = 0;
        BigInteger cur, res, acopy = (*this);
        int sign_a = sign, sign_div = diver.sign;
        BigInteger divider = diver; divider.abs();
        size_t len = body.size();
        while (cur < divider && cur.body.size() < body.size()) { 
            if (!cur.is_zero()) cur.shift_left(1); 
            cur.body[0] = body[len - 1];
            len--;
        }
        for (size_t i = len + 1; i > 0; i--) {
            if (i != len + 1) cur.body[0] = body[i - 1];
            if (cur < divider) {
                if (i == 1) {
                    res.shift_left(1); 
                    res.body[0] = 0;
                    break;
                }
                if (!cur.is_zero()) cur.shift_left(1); 
                res.shift_left(1); 
                res.body[0] = 0;
                continue;
            }
            int mult = cur.search_mult(divider);
            BigInteger prod = divider; 
            prod.mul_short(mult); 
            cur -= prod; 
            res.shift_left(1); 
            res.body[0] = mult;
            if (i > 1 && !cur.is_zero()) cur.shift_left(1); 
        }
        (*this) = res; 
        int diff = 0;
        while (this->body[this->body.size() - diff - 1] == 0 && this->body.size() - diff > 1) {
            diff++;
        } 
        this->body.resize(this->body.size() - diff);  
        sign = sign_a ^ sign_div;
        BigInteger p = diver;
        p *= (*this);
        acopy -= p;
        rem = acopy; 
        return 0;
    } 
};

BigInteger operator+(const BigInteger &a, const BigInteger &b) {
    BigInteger tmp = a;
    tmp += b;
    return tmp;
}

BigInteger operator-(const BigInteger &a, const BigInteger &b) {
    BigInteger tmp = a;
    tmp -= b;
    return tmp;
}

BigInteger operator*(const BigInteger &a, const BigInteger &b) {
    BigInteger tmp = a;
    tmp *= b;
    return tmp;
}

BigInteger operator/(const BigInteger &a, const BigInteger &b) {
    BigInteger tmp = a;
    tmp /= b;
    return tmp;
}

BigInteger operator%(const BigInteger &a, const BigInteger &b) {
    BigInteger tmp = a;
    tmp %= b;
    return tmp;
}

BigInteger operator-(const BigInteger &a) {
    BigInteger ret = a;
    ret.BigIntegerNeg();
    return ret;
}

bool operator==(const BigInteger &a, const BigInteger &b) {
    int res = a.BigIntegerCompare(b);
    return res == 0 ? true : false;
}

bool operator!=(const BigInteger &a, const BigInteger &b) {
    int res = a.BigIntegerCompare(b);
    return res != 0 ? true : false;
}

bool operator<(const BigInteger &a, const BigInteger &b) {
    int res = a.BigIntegerCompare(b);
    return res == -1 ? true : false;
}

bool operator>(const BigInteger &a, const BigInteger &b) {
    int res = a.BigIntegerCompare(b);
    return res == 1 ? true : false;
}

bool operator<=(const BigInteger &a, const BigInteger &b) {
    int res = a.BigIntegerCompare(b);
    return res != 1 ? true : false;
}

bool operator>=(const BigInteger &a, const BigInteger &b) {
    int res = a.BigIntegerCompare(b);
    return res != -1 ? true : false;
}

std::istream& operator>>(std::istream &in, BigInteger &a) {
    std::string s;
    in >> s;
    a = BigInteger(s);
    return in;
}

std::ostream& operator<<(std::ostream &out, const BigInteger &a) {
    std::string s = a.toString();
    out << s;
    return out;
}

std::string& reverse(std::string &s) {
    for (size_t i = 0; i < s.size() / 2; i++) {
        std::swap(s[i], s[s.size() - i - 1]);
    }
    return s;
}

class Rational {
public:
    Rational() {
        numerator = 0;
        denominator = 1;
    }
    
    Rational(const BigInteger &a, const BigInteger &b) {
        numerator = a;
        denominator = b;
        if (denominator < 0) {
            numerator.BigIntegerNeg();
            denominator.BigIntegerNeg();
        }
        (*this).reduce();
    }

    Rational(const BigInteger &a) {
        numerator = a;
        denominator = 1;
    }

    Rational(int a) {
        numerator = a;
        denominator = 1;
    }

    Rational(const Rational &b) {
        numerator = b.numerator;
        denominator = b.denominator;
    }

    Rational& operator=(const Rational &b) {
        numerator = b.numerator;
        denominator = b.denominator;
        return (*this);
    }

    Rational& operator+=(const Rational &b) {
        numerator = numerator * b.denominator + b.numerator * denominator;
        denominator *= b.denominator;
        (*this).reduce();
        return (*this);
    }

    Rational& operator-=(const Rational &b) {
        numerator = numerator * b.denominator - b.numerator * denominator;
        denominator *= b.denominator;
        (*this).reduce();
        return (*this);
    }

    Rational& operator*=(const Rational &b) {
        numerator *= b.numerator;
        denominator *= b.denominator;
        (*this).reduce();
        return (*this);
    }

    Rational& operator/=(const Rational &b) {
        numerator *= b.denominator;
        denominator *= b.numerator;
        if (denominator < 0) {
            numerator.BigIntegerNeg();
            denominator.BigIntegerNeg();
        }
        (*this).reduce();
        return (*this); 
    }

    std::string toString() const { 
        std::string s_numer = numerator.toString();
        if (denominator == 1 || numerator == 0) return s_numer;
        std::string ret; 
        std::string s_denom = denominator.toString();
        ret += s_numer;
        ret += '/';
        ret += s_denom;
        return ret;
    }

    int RationalNeg() {
        numerator.BigIntegerNeg();
        return 0;
    }

    std::string asDecimal(size_t precision=0) {
        bool keepsign = numerator.BigIntegerSign();
        BigInteger numer = numerator;
        numer.abs();
        if (numer == 0) {
            std::string s = numer.toString();
            if (precision != 0) {
                s += '.'; 
                std::string fill(precision, '0');
                s += fill;
            }
            return s;
        }
        BigInteger ten_pwr = 1;
        for (size_t i = 0; i <= precision; i++) {
            ten_pwr *= 10;
        }
        numer *= ten_pwr;
        numer /= denominator;
        BigInteger last_digit = numer % 10;
        numer /= 10;
        if (last_digit >= 5) numer++;
        std::string s = numer.toString();
        if (s.size() < precision) {
            std::string fill(precision - s.size(), '0');
            s = fill + s;
        }
        s.insert(s.size() - precision, ".");
        if (s[s.size() - 1] == '.') s.pop_back();
        else if (s[0] == '.') s = '0' + s; 
        if (keepsign && s != "0") s = '-' + s;
        return s;
    }

    explicit operator double() {
        std::string asdec = this->asDecimal(30);
        double ret = std::stod(asdec);
        return ret;
    }

    friend bool operator==(const Rational &a, const Rational &b);
    friend bool operator!=(const Rational &a, const Rational &b); 
    friend bool operator>(const Rational &a, const Rational &b);
    friend bool operator<(const Rational &a, const Rational &b);
    friend bool operator>=(const Rational &a, const Rational &b);
    friend bool operator<=(const Rational &a, const Rational &b);
private:
    BigInteger numerator;
    BigInteger denominator;

    int reduce() {
        // if (numerator == 0) return 0;
        BigInteger numercp = numerator, denomcp = denominator;
        numercp.abs(), denomcp.abs();
        while (denomcp > 0) {
            BigInteger tmp = numercp;
            numercp = denomcp;
            denomcp = tmp % denomcp;
        }
        numerator /= numercp;
        denominator /= numercp;
        return 0;
    }
};

Rational operator+(const Rational &a, const Rational &b) {
    Rational ret = a;
    ret += b;
    return ret;
}

Rational operator-(const Rational &a, const Rational &b) {
    Rational ret = a;
    ret -= b;
    return ret;
}

Rational operator*(const Rational &a, const Rational &b) {
    Rational ret = a;
    ret *= b;
    return ret;
}

Rational operator/(const Rational &a, const Rational &b) {
    Rational ret = a;
    ret /= b;
    return ret;
}

Rational operator-(const Rational &a) {
    Rational ret = a;
    ret.RationalNeg();
    return ret;
}

bool operator==(const Rational &a, const Rational &b) {
    return (a.numerator == b.numerator && a.denominator == b.denominator);
}

bool operator!=(const Rational &a, const Rational &b) {
    return (a.numerator != b.numerator || a.denominator != b.denominator);
}

bool operator<(const Rational &a, const Rational &b) {
    return (a.numerator * b.denominator < b.numerator * a.denominator);
}

bool operator>(const Rational &a, const Rational &b) {
    return (a.numerator * b.denominator > b.numerator * a.denominator);
}

bool operator<=(const Rational &a, const Rational &b) {
    return (a.numerator * b.denominator <= b.numerator * a.denominator);
}

bool operator>=(const Rational &a, const Rational &b) {
    return (a.numerator * b.denominator >= b.numerator * a.denominator);
}

std::istream& operator>>(std::istream &in, Rational &a) {
    BigInteger n, d;
    in >> n >> d;
    a = Rational(n, d);
    return in;
}

std::ostream& operator<<(std::ostream &out, const Rational &a) {
    std::string s = a.toString();
    out << s;
    return out;
}
