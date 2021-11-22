#include <iostream>
#include <vector>
#include <string>

static const std::string DIGITS = "0123456789";
static const int RADIX = 1 << 15;
std::string& reverse(std::string& s);

class BigInteger;
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
        if (body.size() < b.body.size()) {
            body.resize(b.body.size());
        }
        int carry = 0;
        for (size_t i = 0; i < b.body.size(); i++) {
            int sum = body[i] + b.body[i] + carry;
            body[i] = sum % RADIX;
            carry = sum / RADIX;
        }
        if (carry) {
            body.resize(body.size() + 1);
            int index = 0;
            while (carry) {
                int sum = body[b.body.size() + index] + carry;
                body[b.body.size() + index] = sum % RADIX;
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
        if (sign ^ b.sign == 0) { 
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
        BigInteger res;
        int keepsign = sign ^ b.sign;
        for (size_t i = 0; i < b.body.size(); i++) {
            BigInteger tmp((*this));
            tmp.mul_short(b.body[i]);
            tmp.shift_left(i);
            res += tmp; 
        }
        (*this) = res; 
        sign = keepsign;
        return (*this);
    }

    BigInteger& operator/=(const BigInteger &divider) {
        BigInteger rem;
        (*this).div(divider, rem);
        return (*this);
    }

    BigInteger& operator%=(const BigInteger &divider) {
        BigInteger rem;
        (*this).div(divider, rem);
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

    operator bool() {
        return this->is_zero() ? false : true; 
    }

    int BigIntegerNeg() {
        sign ^= 1;
        return 0;
    }

    int BigIntegerCompare(const BigInteger &b) const {
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

private:
    bool sign;
    std::vector <int> body;

    bool is_zero() const {
        return body.size() == 1 && body[0] == 0 ? true : false;
    } 
    int abs() {
        sign = 0;
        return 0;
    }
    
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
        BigInteger cur, res, acopy = (*this);
        int sign_a = sign, sign_div = diver.sign;
        BigInteger divider = diver; divider.abs();
        int len = body.size();
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
    int x;
    in >> x;
    a = x;
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


