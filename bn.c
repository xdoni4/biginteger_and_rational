#include <stdlib.h>
// #include "bn.h"

static const char DIGITS[36] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
static const int RADIX = 1 << 15;

enum bn_codes {
BN_OK, BN_NULL_OBJECT, BN_NO_MEMORY, BN_DIVIDE_BY_ZERO
};

struct bn_s {
    int *body;
    int bodysize;
    int sign;
};

int bn_mul_short(bn *t, int int_mult);
int is_zero(bn const *t);
int bn_init_string_radix(bn *t, const char *init_string, int radix);
int match_size(bn *t, int size) {
    if (t == NULL) return BN_NULL_OBJECT;
    t->body = realloc(t->body, size*sizeof(int));
    if (t->body == NULL) {
        bn_delete(t);
        return BN_NO_MEMORY;
    }
    for (size_t i = t->bodysize; i < size; i++) {
        t->body[i] = 0;
    }
    t->bodysize = size;
    return BN_OK;
}

int expand(bn *t) {
    if (t == NULL) return BN_NULL_OBJECT;
    int *newbody = malloc(sizeof(int) * (t->bodysize << 1));
    if (newbody == NULL) {
        bn_delete(t);
        return BN_NO_MEMORY;
    }
    for (size_t i = 0; i < t->bodysize; i++) {
        newbody[i] = t->body[i];
    }
    t->bodysize <<= 1;
    free(t->body);
    t->body = newbody;
    return BN_OK;
}

char *expand_string(char *s, size_t *s_cap) {
    if (s == NULL) return NULL;
    char *news = malloc(*s_cap << 1);
    if (news == NULL) {
        free(s);
        return NULL;
    }
    for (size_t i = 0; i < *s_cap; i++) {
        news[i] = s[i];
    }
    *s_cap <<= 1;
    free(s);
    return news;
}

char *reverse(char *s, size_t slen) {
    if (s == NULL) return NULL;
    char *ret = malloc(slen);
    if (ret == NULL) return NULL;
    for (size_t i = slen; i > 0; i--) {
        ret[slen-i] = s[i - 1];
    }
    free(s);
    return ret;
}

bn *bn_new() {
    bn *r = malloc(sizeof(bn));
    if (r == NULL) return NULL;
    r->bodysize = 1;
    r->sign = 0;
    r->body = malloc(sizeof(int) * r->bodysize);
    if (r->body == NULL) {
        free(r);
        return NULL;
    }
    r->body[0] = 0;
    return r;
}

bn *bn_init(bn const *orig) {
    bn *r = malloc(sizeof(bn));
    if (r == NULL) return NULL;
    r->bodysize = orig->bodysize;
    r->sign = orig->sign;
    r->body = malloc(sizeof(int) * r->bodysize);
    if (r->body == NULL) {
        free(r);
        return NULL;
    }
    for (size_t i = 0; i < r->bodysize; i++) {
        r->body[i] = orig->body[i];
    }
    return r;
}

int bn_init_int(bn *t, int init_int) {
    t->sign = -(init_int >> 31);
    init_int = init_int < 0 ? -init_int : init_int;
    size_t index = 0;
    for (; init_int > 0; init_int /= RADIX) {
        int rem = init_int % RADIX;
        if (index >= t->bodysize) {
            int code = expand(t);
            if (code != BN_OK) {
                bn_delete(t);
                return code;
            }
        }
        t->body[index] = rem;
        index++;
    }
    return BN_OK;
}

int bn_init_string(bn *t, const char *init_string) {
    int code = bn_init_string_radix(t, init_string, 10);
    if (code != BN_OK) return code; 
    return BN_OK;
}

int bn_init_string_radix(bn *t, const char *init_string, int radix) {
    if (t == NULL) return BN_NULL_OBJECT;
    bn *radix_pow = bn_new();
    if (radix_pow == NULL) { 
        bn_delete(t);
        return BN_NO_MEMORY;
    }
    int code = bn_init_int(radix_pow, 1);
    if (code != BN_OK) {
        bn_delete(t);
        bn_delete(radix_pow);
        return code;
    }
    size_t size = 0;
    while (init_string[size] != '\0') size++; 
    for (size_t i = size; i > 0; i--) {
        int digit;
        if (init_string[i - 1] == '-') break;
        if (init_string[i - 1] >= '0' && init_string[i - 1] <= '9') {
            digit = init_string[i - 1] - '0';
        }
        else digit = init_string[i - 1] - 'A' + 10;
        bn *prod = bn_init(radix_pow);
        if (prod == NULL) {
            bn_delete(prod);
            bn_delete(radix_pow);
            bn_delete(t);
            return BN_NO_MEMORY;
        }
        code = bn_mul_short(prod, digit);
        if (code != BN_OK) {
            bn_delete(prod);
            bn_delete(radix_pow);
            bn_delete(t);
            return code;
        }
        code = bn_add_to(t, prod);
        if (code != BN_OK) {
            bn_delete(prod);
            bn_delete(radix_pow);
            bn_delete(t);
            return code;
        }
        code = bn_mul_short(radix_pow, radix);
        if (code != BN_OK) {
            bn_delete(prod);
            bn_delete(radix_pow);
            bn_delete(t);
            return code;
        }
        bn_delete(prod);
    }
    t->sign = init_string[0] == '-' ? 1 : 0;
    bn_delete(radix_pow);
    return BN_OK;
}

int bn_copy_to(bn *t, bn *orig) {
    if (t == NULL) return BN_NULL_OBJECT;
    t->sign = orig->sign;
    t->bodysize = orig->bodysize;
    t->body = realloc(t->body, sizeof(int) * t->bodysize);
    if (t->body == NULL) {
        bn_delete(t);
        return BN_NO_MEMORY;
    }
    for (size_t i = 0; i < t->bodysize; i++) {
        t->body[i] = orig->body[i];
    }
    return BN_OK;
}

int bn_delete(bn *t) {
    if (t == NULL) return BN_OK;
    free(t->body);
    t->body = NULL;
    free(t);
    t = NULL;
    return BN_OK;
}

int bn_cmp(bn const *left, bn const *right) {
    if (left == NULL || right == NULL) return BN_NULL_OBJECT;
    if (left->sign ^ right->sign) {
        if (left->sign) {
            return -1;
        }
        else {
            return 1;
        }
    }
    if (left->sign) {
        if (left->bodysize > right->bodysize) {
            return -1;
        }
        else if (left->bodysize < right->bodysize) {
            return 1;
        }
        else {
            for (size_t i = left->bodysize; i > 0; i--) {
                if (left->body[i - 1] > right->body[i - 1]){
                    return -1;
                }
                else if (left->body[i - 1] < right->body[i - 1]) {
                    return 1;
                }
            }
            return 0;
        }
    }
    if (left->bodysize > right->bodysize) {
        return 1;
    }
    else if (left->bodysize < right->bodysize) {
        return -1;
    }
    else {
        for (size_t i = left->bodysize; i > 0; i--) {
            if (left->body[i - 1] > right->body[i - 1]) {
                return 1;
            }
            else if (left->body[i - 1] < right->body[i - 1]) {
                return -1;
            }
        }
        return 0;
    }
}

int bn_neg(bn *t) {
    if (t == NULL) return BN_NULL_OBJECT;
    t->sign ^= 1;
    return 0;
}

int bn_abs(bn *t) {
    if (t == NULL) return BN_NULL_OBJECT;
    t->sign = 0;
    return 0;
}

int bn_sign(bn const *t) {
    if (t == NULL) return BN_NULL_OBJECT;
    if (is_zero(t)) return 0;
    else if (t->sign) return -1;
    else return 1;
}

int is_zero(bn const *t) {
    if (t == NULL) return BN_NULL_OBJECT;
    return t->bodysize == 1 && t->body[0] == 0 ? 1 : 0;
}

int bn_shift_left(bn *t, size_t k) {
    if (t == NULL) return BN_NULL_OBJECT;
    if (k == 0) return BN_OK;
    t->bodysize += k;
    t->body = realloc(t->body, t->bodysize * sizeof(int));
    if (t->body == NULL) {
        bn_delete(t);
        return BN_NO_MEMORY;
    }
    for (size_t i = 0; i < k; i++) {
        t->body[t->bodysize - k + i] = 0;
    }
    for (size_t i = t->bodysize; i > k; i--) {
        t->body[i - 1] = t->body[i - 1 - k];
        t->body[i - 1 - k] = 0;
    }
    return BN_OK;
}

int bn_mul_short(bn *t, int int_mult) {
    if (t == NULL) return BN_NULL_OBJECT;
    t->sign ^= -(int_mult >> 31);
    int_mult = int_mult < 0 ? -int_mult : int_mult;
    int carry = 0;
    for (size_t i = 0; i < t->bodysize; i++) {
        int product = t->body[i] * int_mult + carry;
        t->body[i] = product % RADIX;
        carry = product / RADIX;
    }
    if (carry) {
        t->bodysize++;
        t->body = realloc(t->body, t->bodysize * sizeof(int));
        if (t->body == NULL) {
            bn_delete(t);
            return BN_NO_MEMORY;
        }
        t->body[t->bodysize - 1] = carry;
    }
    return BN_OK;
}

int bn_div_short(bn *t, int int_div, int *int_rem) {
    if (t == NULL) return BN_NULL_OBJECT;
    t->sign ^= -(int_div >> 31);
    int_div = int_div < 0 ? -int_div : int_div;
    if (int_div == 0) return BN_DIVIDE_BY_ZERO;
    if (t->bodysize == 1) {
        *int_rem = t->body[0] % int_div;
        t->body[0] /= int_div;
        return BN_OK;
    }
    int curint = 0;
    for (size_t i = t->bodysize; i > 0; i--) {
        curint += t->body[i - 1];
        if (curint < int_div) {
            t->body[i - 1] = 0;
            curint *= RADIX;
            continue;
        }
        t->body[i - 1] = curint / int_div;
        curint %= int_div;
        curint *= RADIX;
    }
    curint /= RADIX;
    *int_rem = curint;
    while (t->body[t->bodysize - 1] == 0 && t->bodysize > 1) {
        t->bodysize--;
    }
    t->body = realloc(t->body, t->bodysize * sizeof(int));
    if (t->body == NULL) {
        bn_delete(t);
        return BN_NO_MEMORY;
    }
    return BN_OK;
}

int search_mult(const bn *t, const bn *divider) {
    if (t == NULL || divider == NULL) {
        return BN_NULL_OBJECT;
    }
    int l = 0, r = RADIX;
    while (l + 1 < r) {
        int m = (l + r) / 2;
        bn *prod = bn_init(divider);
        if (prod == NULL) {
            return BN_NO_MEMORY;
        }
        int code = bn_mul_short(prod, m);
        if (code != BN_OK) {
            bn_delete(prod);
            return code;
        }
        code = bn_cmp(prod, t);
        bn_delete(prod);
        if (code == -1) {
            l = m;
        }
        else if (code == 0) {
            return m;
        }
        else if (code == 1){
            r = m;
        }
        else {
            return code;
        }
    }
    bn *prod = bn_init(divider);
    if (prod == NULL) {
        return BN_NO_MEMORY;
    }
    int code = bn_mul_short(prod, r);
    if (code != BN_OK) {
        bn_delete(prod);
        return code;
    }
    int ret = bn_cmp(prod, t) == 1 ? l : r;
    bn_delete(prod);
    return ret;
}

int bn_add_to(bn *t, bn const *right) {
    int code = BN_OK;
    if (t == NULL || right == NULL) return BN_NULL_OBJECT;
    if (t->sign ^ right->sign) { 
        bn *t_abs = bn_init(t);
        if (t_abs == NULL) return BN_NO_MEMORY;
        bn_abs(t_abs);
        bn *right_abs = bn_init(right);
        if (right_abs == NULL) {
            bn_delete(t_abs);
            return BN_NO_MEMORY;
        }
        bn_abs(right_abs);
        if (bn_cmp(t_abs, right_abs) == 0) {
            bn *zero = bn_new();
            if (zero == NULL) {
                bn_delete(t_abs);
                bn_delete(right_abs);
                return BN_NO_MEMORY;
            }
            code = bn_copy_to(t, zero);
            if (code != BN_OK) {
                bn_delete(t_abs);
                bn_delete(right_abs);
                return code;
            }
            bn_delete(zero);
            bn_delete(t_abs);
            bn_delete(right_abs);
            return BN_OK;
        }
        if (bn_cmp(t_abs, right_abs) == 1) {
            int sign = t->sign;
            code = bn_sub_to(t_abs, right_abs);
            if (code != BN_OK) {
                bn_delete(t_abs);
                bn_delete(right_abs);
                return code;
            }
            code = bn_copy_to(t, t_abs);
            if (code != BN_OK) {
                bn_delete(t_abs);
                bn_delete(right_abs);
                return code;
            }
            t->sign = sign;
            bn_delete(t_abs);
            bn_delete(right_abs);
            return BN_OK;
        }
        int sign = right->sign;
        code = bn_sub_to(right_abs, t_abs);
        if (code != BN_OK) {
            bn_delete(t_abs);
            bn_delete(right_abs);
            return code;
        }
        code = bn_copy_to(t, right_abs);
        if (code != BN_OK) {
            bn_delete(t_abs);
            bn_delete(right_abs);
            return code;
        }
        t->sign = sign;
        if (is_zero(t)) t->sign = 0;
        bn_delete(t_abs);
        bn_delete(right_abs);
        return BN_OK;
    }
    if (t->bodysize < right->bodysize) {
        code = match_size(t, right->bodysize);
        if (code != BN_OK) {
            bn_delete(t);
            return code;
        }
    }
    int carry = 0;
    for (size_t i = 0; i < right->bodysize; i++) {
        int sum = t->body[i] + right->body[i] + carry;
        t->body[i] = sum % RADIX;
        carry = sum / RADIX;
    }
    if (carry) {
        code = match_size(t, t->bodysize + 1);
        if (code != BN_OK) {
            bn_delete(t);
            return code;
        }
        int index = 0;
        while (carry) {
            int sum = t->body[right->bodysize + index] + carry;
            t->body[right->bodysize + index] = sum % RADIX;
            carry = sum / RADIX;
            index++;
        }
        int diff = 0;
        while (t->body[t->bodysize - diff - 1] == 0 && t->bodysize - diff > 1) {
            diff++;
        }
        code = match_size(t, t->bodysize - diff);
        if (code != BN_OK) {
            bn_delete(t);
            return code;
        }
    }
    return BN_OK;
}

int bn_sub_to(bn *t, bn const *right) {
    if (t == NULL || right == NULL) return BN_NULL_OBJECT;
    int code = BN_OK;
    if (t->sign ^ right->sign == 0) { 
        bn *t_abs = bn_init(t);
        if (t_abs == NULL) return BN_NO_MEMORY;
        bn_abs(t_abs);
        bn *right_abs = bn_init(right);
        if (right_abs == NULL) {
            bn_delete(t_abs);
            return BN_NO_MEMORY;
        }
        bn_abs(right_abs); 
        if (bn_cmp(t_abs, right_abs) == 0) {
            bn *zero = bn_new();
            if (zero == NULL) {
                bn_delete(t_abs);
                bn_delete(right_abs);
                return BN_NO_MEMORY;
            }
            code = bn_copy_to(t, zero);
            if (code != BN_OK) {
                bn_delete(t_abs);
                bn_delete(right_abs);
                return code;
            }
            bn_delete(zero);
            bn_delete(t_abs);
            bn_delete(right_abs);
            return BN_OK;
        }
        if (bn_cmp(t_abs, right_abs) == 1) {
            int sign = t->sign;
            int carry = 0;
            for (size_t i = 0; i < right_abs->bodysize; i++) {
                t_abs->body[i] -= carry;
                carry = 0;
                t_abs->body[i] -= right_abs->body[i];
                if (t_abs->body[i] < 0) {
                    t_abs->body[i] += RADIX;
                    carry++;
                }
            }
            int index = right_abs->bodysize;
            while (carry > 0 ) {
                t_abs->body[index] -= carry;
                carry = 0;
                if (t_abs->body[index] < 0) {
                    t_abs->body[index] += RADIX;
                    carry++;
                }
                index++;
            }
            int diff = 0;
            while (t_abs->body[t_abs->bodysize - diff - 1] == 0 && t_abs->bodysize - diff > 1) {
                diff++;
            }
            match_size(t_abs, t_abs->bodysize - diff);
            if (code != BN_OK) {
                bn_delete(t);
                bn_delete(t_abs);
                bn_delete(right_abs);
                return code;
            }
            code = bn_copy_to(t, t_abs);
            if (code != BN_OK) {
                bn_delete(t);
                bn_delete(t_abs);
                bn_delete(right_abs);
                return code;
            } 
            t->sign = sign;
            bn_delete(t_abs);
            bn_delete(right_abs);
            return BN_OK;
        }
        int sign = t->sign;
        int carry = 0;
        for (size_t i = 0; i < t_abs->bodysize; i++) {
            right_abs->body[i] -= carry;
            carry = 0;
            right_abs->body[i] -= t_abs->body[i];
            if (right_abs->body[i] < 0) {
                right_abs->body[i] += RADIX;
                carry++;
            }
        }
        int index = t_abs->bodysize;
        while (carry > 0) {
            right_abs->body[index] -= carry;
            carry = 0;
            if (right_abs->body[index] < 0) {
                right_abs->body[index] += RADIX;
                carry++;
            }
            index++;
        }
        int diff = 0;
        while (right_abs->body[right_abs->bodysize - diff - 1] == 0 && right_abs->bodysize - diff > 1) {
            diff++;
        } 
        code = match_size(right_abs, right_abs->bodysize - diff);
        if (code != BN_OK) {
            bn_delete(t);
            bn_delete(t_abs);
            bn_delete(right_abs);
            return code;
        }  
        code = bn_copy_to(t, right_abs);
        if (code != BN_OK) {
            bn_delete(t);
            bn_delete(t_abs);
            bn_delete(right_abs);
            return code;
        }
        t->sign = sign ^ 1;
        bn_delete(t_abs);
        bn_delete(right_abs);
        if (is_zero(t)) t->sign = 0;
        return BN_OK;
    }
    int sign = t->sign;
    bn *t_abs = bn_init(t);
    if (t_abs->body == NULL) return BN_NO_MEMORY;
    bn_abs(t_abs);
    bn *right_abs = bn_init(right);
    if (right_abs->body == NULL) {
        bn_delete(t_abs);
        return BN_NO_MEMORY;
    }
    bn_abs(right_abs);
    code = bn_add_to(t_abs, right_abs);
    if (code != BN_OK) {
        bn_delete(t_abs);
        bn_delete(right_abs);
        return code;
    }
    code = bn_copy_to(t, t_abs);
    if (code != BN_OK) {
        bn_delete(t);
        bn_delete(t_abs);
        bn_delete(right_abs);
        return code;
    }
    t->sign = sign;
    bn_delete(t_abs);
    bn_delete(right_abs);
    return BN_OK;
}

int bn_mul_to(bn *t, bn const *right) {
    if (t == NULL || right == NULL) return BN_NULL_OBJECT;
    bn *res = bn_new();
    int code = BN_OK;
    if (res == NULL) return BN_NO_MEMORY;
    int sign = t->sign ^ right->sign;
    for (size_t i = 0; i < right->bodysize; i++) {
        bn *tmp = bn_init(t);
        if (tmp == NULL) {
            bn_delete(res);
            return BN_NO_MEMORY;
        }
        code = bn_mul_short(tmp, right->body[i]);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(tmp);
            return code;
        }
        code = bn_shift_left(tmp, i);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(tmp);
            return code;
        }
        code = bn_add_to(res, tmp);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(tmp);
            return code;
        }
        bn_delete(tmp);
    }
    code = bn_copy_to(t, res);
    if (code != BN_OK) {
        bn_delete(t);
        bn_delete(res);
        return code;
    }
    t->sign = sign;
    bn_delete(res);
    return BN_OK;
}

int bn_div_internal(bn *t, bn const *divider, bn *rem) {
    if (t == NULL || divider == NULL || rem == NULL) return BN_NULL_OBJECT;
    if (is_zero(divider)) return BN_DIVIDE_BY_ZERO;
    bn *cur = bn_new();
    if (cur == NULL) return BN_NO_MEMORY;
    bn *res = bn_new();
    if (res == NULL) {
        bn_delete(cur);
        return BN_NO_MEMORY;
    }
    int code = BN_OK;
    int sign_t = t->sign, sign_div = divider->sign;
    bn_abs(divider);
    int len = t->bodysize;
    while (bn_cmp(cur, divider) < 0 && cur->bodysize < t->bodysize) { 
        if (is_zero(cur) == 0) code = bn_shift_left(cur, 1);
        if (code != BN_OK) {
            bn_delete(cur);
            bn_delete(res);
            return code;
        }
        cur->body[0] = t->body[len - 1];
        len--;
    }
    for (size_t i = len + 1; i > 0; i--) {
        if (i != len + 1) cur->body[0] = t->body[i - 1];
        if (bn_cmp(cur, divider) < 0) {
            if (i == 1) {
                code = bn_shift_left(res, 1);
                if (code != BN_OK) {
                    bn_delete(res);
                    bn_delete(cur);
                    return code;
                }
                res->body[0] = 0;
                break;
            }
            if (is_zero(cur) == 0) code = bn_shift_left(cur, 1);
            if (code != BN_OK) {
                bn_delete(cur);
                bn_delete(res);
                return code;
            }
            code = bn_shift_left(res, 1);
            if (code != BN_OK) {
                bn_delete(res);
                bn_delete(cur);
                return code;
            }
            res->body[0] = 0;
            continue;
        }
        int mult = search_mult(cur, divider);
        bn *prod = bn_init(divider);
        if (prod == NULL) {
            bn_delete(cur);
            bn_delete(res);
            return BN_NO_MEMORY;
        }
        code = bn_mul_short(prod, mult);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(cur);
            bn_delete(prod);
            return code;
        }
        code = bn_sub_to(cur, prod);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(cur);
            bn_delete(prod);
            return code;
        }
        code = bn_shift_left(res, 1);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(cur);
            bn_delete(prod);
            return code;
        }
        res->body[0] = mult;
        bn_delete(prod);
        if (i > 1 && is_zero(cur) == 0) code = bn_shift_left(cur, 1);
        if (code != BN_OK) {
            bn_delete(res);
            bn_delete(cur);
            return code;
        }
    }
    code = bn_copy_to(t, res);
    if (code != BN_OK) {
        bn_delete(t);
        bn_delete(res);
        bn_delete(cur);
        return code;
    }
    int diff = 0;
    while (t->body[t->bodysize - diff - 1] == 0 && t->bodysize - diff > 1) {
        diff++;
    }
    match_size(t, t->bodysize - diff);
    /* t->body = realloc(t->body, t->bodysize * sizeof(int));
    if (t->body == NULL) {
        bn_delete(t);
        bn_delete(res);
        bn_delete(cur);
        return BN_NO_MEMORY;
    } */
    code = bn_copy_to(rem, cur);
    if (code != BN_OK) {
        bn_delete(rem);
        bn_delete(res);
        bn_delete(cur);
        return code;
    }
    diff = 0;
    while (rem->body[rem->bodysize - diff - 1] == 0 && rem->bodysize - diff > 1) {
        diff++;
    }
    match_size(t, t->bodysize - diff);
    /* rem->body = realloc(rem->body, rem->bodysize * sizeof(int));
    if (rem->body == NULL) {
        bn_delete(rem);
        bn_delete(res);
        bn_delete(cur);
        return BN_NO_MEMORY;
    } */
    int sign = sign_t ^ sign_div;
    bn *zero = bn_new();
    if (zero == NULL) {
        bn_delete(cur);
        bn_delete(res);
        return BN_NO_MEMORY;
    }
    if (bn_cmp(rem, zero) == 0) {
        t->sign = sign;
        bn_delete(cur);
        bn_delete(res);
        bn_delete(zero);
        return BN_OK;
    }
    bn_delete(zero);
    if (sign) {
        bn *one = bn_new();
        if (one == NULL) {
            bn_delete(res);
            bn_delete(cur);
            return BN_NO_MEMORY;
        }
        code = bn_init_int(one, 1);
        if (code != BN_OK) {
            bn_delete(one);
            bn_delete(res);
            bn_delete(cur);
            return code;
        }
        code = bn_add_to(t, one);
        if (code != BN_OK) {
            bn_delete(t);
            bn_delete(one);
            bn_delete(res);
            bn_delete(cur);
            return code;
        }
        bn_delete(one);
        code = bn_sub_to(rem, divider);
        if (code != BN_OK) {
            bn_delete(rem);
            bn_delete(res);
            bn_delete(cur);
            return code;
        }
        if (sign_t) {
            bn_abs(rem);
        }
        t->sign = 1;
    }
    else {
        if (sign_div) {
            bn_neg(rem);
        }
    }
    bn_delete(cur);
    bn_delete(res);
    return BN_OK;
}

int bn_div_to(bn *t, bn const *right) {
    bn *rem = bn_new();
    int code = bn_div_internal(t, right, rem);
    bn_delete(rem);
    return code;
}

int bn_mod_to(bn *t, bn const *right) {
    bn *rem = bn_new();
    int code = bn_div_internal(t, right, rem);
    if (code != BN_OK) {
        bn_delete(rem);
        return code;
    }
    code = bn_copy_to(t, rem);
    if (code != BN_OK) {
        bn_delete(t);
        bn_delete(rem);
        return code;
    }
    bn_delete(rem);
    return BN_OK;
}

bn* bn_add(bn const *left, bn const *right) {
    bn *left_copy = bn_init(left);
    if (left_copy == NULL) return NULL;
    int code = bn_add_to(left_copy, right);
    if (code != BN_OK) {
        bn_delete(left_copy);
        return NULL;
    }
    bn *ret = bn_init(left_copy);
    if (ret == NULL) {
        bn_delete(left_copy);
        return NULL;
    }
    bn_delete(left_copy);
    return ret;
}

bn* bn_sub(bn const *left, bn const *right) {
    bn *left_copy = bn_init(left);
    if (left_copy == NULL) return NULL;
    int code = bn_sub_to(left_copy, right);
    if (code != BN_OK) {
        bn_delete(left_copy);
        return NULL;
    }
    bn *ret = bn_init(left_copy);
    bn_delete(left_copy);
    return ret;
}

bn* bn_mul(bn const *left, bn const *right) {
    bn *left_copy = bn_init(left);
    if (left_copy == NULL) return NULL;
    int code = bn_mul_to(left_copy, right);
    if (code != BN_OK) {
        bn_delete(left_copy);
        return NULL;
    }
    bn *ret = bn_init(left_copy);
    bn_delete(left_copy);
    return ret;
}

bn* bn_div(bn const *left, bn const *right) {
    bn *left_copy = bn_init(left);
    if (left_copy == NULL) return NULL;
    int code = bn_div_to(left_copy, right);
    if (code != BN_OK) {
        bn_delete(left_copy);
        return NULL;
    }
    bn *ret = bn_init(left_copy);
    bn_delete(left_copy);
    return ret;   
}

bn* bn_mod(bn const *left, bn const *right) {
    bn *left_copy = bn_init(left);
    if (left_copy == NULL) return NULL;
    int code = bn_mod_to(left_copy, right);
    if (code != BN_OK) {
        bn_delete(left_copy);
        return NULL;
    }
    bn *ret = bn_init(left_copy);
    bn_delete(left_copy);
    return ret;   
}

int bn_pow_to(bn *t, int degree) {
    if (t == NULL) return BN_NULL_OBJECT;
    int *arr = malloc(sizeof(int) * 128);
    if (arr == NULL) return BN_NO_MEMORY;
    int size = 0;
    while (degree > 1) {
        arr[size++] = degree & 1;
        degree = degree & 1 ? degree - 1 : degree >> 1;
    }
    bn *res = bn_init(t);
    if (res == NULL) return BN_NO_MEMORY;
    int code = BN_OK;
    while (size > 0) {
        if (arr[--size]) {
            code = bn_mul_to(res, t);
            if (code != BN_OK) {
                bn_delete(res);
                return code;
            }
        }
        else {
            code = bn_mul_to(res, res);
            if (code != BN_OK) {
                bn_delete(res);
                return code;
            }
        }
    }
    code = bn_copy_to(t, res);
    if (code != BN_OK) {
        bn_delete(t);
        bn_delete(res);
        return code;
    }
    bn_delete(res);
    free(arr);
    return BN_OK;
}

//Heron
int bn_root_to(bn *t, int reciprocal) {
    if (t == NULL) return BN_NULL_OBJECT;
    if (reciprocal & 1 == 0 && t->sign) {
        return BN_NULL_OBJECT;
    } 
    bn *x = bn_new();
    if (x == NULL) return BN_NO_MEMORY;
    int code = bn_init_int(x, RADIX);
    if (code != BN_OK) {
        bn_delete(x);
        return code;
    }
    code = bn_pow_to(x, t->bodysize/2);
    if (code != BN_OK) {
        bn_delete(x);
        return code;
    }
    bn *xprev = bn_new();
    if (xprev == NULL) {
        bn_delete(x);
        return BN_NO_MEMORY;
    }
    int decreased = 0;
    for (;;) {
        bn_copy_to(xprev, x);
        if (xprev == NULL) {
            bn_delete(x);
            return BN_NO_MEMORY;
        }
        bn *pow_x = bn_init(x);
        if (pow_x == NULL) {
            bn_delete(x);
            return BN_NO_MEMORY;
        }
        code = bn_pow_to(pow_x, reciprocal - 1);
        if (code != BN_OK) {
            bn_delete(pow_x);
            bn_delete(x);
            return code;
        }
        bn *adiv = bn_div(t, pow_x);
        if (adiv == NULL) {
            bn_delete(x);
            bn_delete(pow_x);
            return BN_NO_MEMORY;
        }
        code = bn_mul_short(x, reciprocal - 1);
        if (code != BN_OK) {
            bn_delete(pow_x);
            bn_delete(x);
            bn_delete(adiv);
            return code;
        }
        code = bn_add_to(x, adiv);
        bn_delete(adiv);
        bn_delete(pow_x);
        if (code != BN_OK) {
            bn_delete(x);
            return code;
        }
        int rem = 0, *ptr_rem = &rem;
        int code = bn_div_short(x, reciprocal, ptr_rem);
        if (code != BN_OK) {
            bn_delete(x);
            return BN_NO_MEMORY;
        }
        if (decreased == 1 && bn_cmp(xprev, x) == -1 || bn_cmp(xprev, x) == 0) break;
        decreased = bn_cmp(xprev, x);
    }
    code = bn_copy_to(t, x);
    bn_delete(x);
    if (code != BN_OK) {
        bn_delete(t);
        return code;
    }
    bn_delete(xprev);
    return BN_OK;
}

int search_borders(bn *t, bn *l, bn *r, int reciprocal) {
    bn *res = bn_new();
    if (res == NULL) {
        return BN_NO_MEMORY;
    }
    bn *prev_res = bn_new();
    if (prev_res == NULL) {
        bn_delete(res);
        return BN_NO_MEMORY;
    }
    int code = bn_init_string_radix(res, "6915777066001312435749111729505866337794078078854838235388098563816415549302886859766882315063491277264165706337993904451643926346699325877763785008527893949907444625673453768557648431599540094251747988131521121167105079170679299025089234953012202931877228134203758585426002383689915168166737312374278767231671603099410564945030041594030167397451582120484610559509620893362094245611235867388290287669029170175208660535466702201384538978677854258270479109399444273248538028357932212557811493873987052329401574602963086996791793584200843628100539289022183044416497869451115727312536464362783753562055934743529781001228665471168880353932803439386640377988872314787337425378188316763371654428959859941779754591276957328093131352360479841426321494807650075372467986974304979930628399048452727501460766208587332778980045473334277224276816621130503735309601343422738", 10);
    if (code != BN_OK) {
        bn_delete(res);
        bn_delete(prev_res);
        return code;
    }
    bn *pow_res = bn_init(res);
    if (pow_res == NULL) {
        bn_delete(res);
        bn_delete(prev_res);
        return BN_NO_MEMORY;
    }
    code = bn_pow_to(pow_res, reciprocal);
    if (code != BN_OK) {
        bn_delete(pow_res);
        bn_delete(res);
        bn_delete(prev_res);
        return code;
    }
    while (bn_cmp(pow_res, t) == -1) {
        code = bn_copy_to(prev_res, res);
        if (code != BN_OK) {
            bn_delete(prev_res);
            bn_delete(res);
            bn_delete(pow_res);
            return code;
        }
        code = bn_pow_to(res, 2);
        if (code != BN_OK) {
            bn_delete(prev_res);
            bn_delete(res);
            bn_delete(pow_res);
            return code;
        }
        code = bn_copy_to(pow_res, res);
        if (code != BN_OK) {
            bn_delete(prev_res);
            bn_delete(res);
            bn_delete(pow_res);
            return code;
        }
        code = bn_pow_to(pow_res, reciprocal);
        if (code != BN_OK) {
            bn_delete(prev_res);
            bn_delete(res);
            bn_delete(pow_res);
            return code;
        }
    }
    code = bn_copy_to(l, prev_res);
    if (code != BN_OK) {
        bn_delete(prev_res);
        bn_delete(res);
        bn_delete(pow_res);
        return code;
    }
    code = bn_copy_to(r, res);
    if (code != BN_OK) {
        bn_delete(prev_res);
        bn_delete(res);
        bn_delete(pow_res);
        return code;
    }
    bn_delete(pow_res);
    bn_delete(res);
    bn_delete(prev_res);
    return BN_OK;
}

int bn_root_to_binary_search(bn *t, int reciprocal) {
    if (t == NULL) return BN_NULL_OBJECT;
    if (reciprocal & 1 == 0 && t->sign) {
        return BN_NULL_OBJECT;
    }
    bn *l = bn_new();
    if (l == NULL) {
        return BN_NO_MEMORY;
    }
    bn *r = bn_new();
    if (r == NULL) {
        bn_delete(l);
        return BN_NO_MEMORY;
    }
    int code = search_borders(t, l, r, reciprocal);
    if (code != BN_OK) {
        bn_delete(l);
        bn_delete(r);
        return code;
    }
    bn *loopl = bn_init(l);
    if (loopl == NULL) {
        bn_delete(l);
        bn_delete(r);
        return BN_NO_MEMORY;
    }
    bn *one = bn_new();
    if (one == NULL) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        return BN_NO_MEMORY; 
    }
    bn *two = bn_new();
    if (two == NULL) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        bn_delete(one);
        return BN_NO_MEMORY;
    }
    code = bn_init_int(one, 1);
    if (code != BN_OK) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        bn_delete(one);
        bn_delete(two);
        return code;
    }
    code = bn_init_int(two, 2);
    if (code != BN_OK) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        bn_delete(one);
        bn_delete(two);
        return code;
    } 
    code = bn_add_to(loopl, one);
    if (code != BN_OK) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        bn_delete(one);
        bn_delete(two);
        return code;
    }
    while (bn_cmp(loopl, r) == -1) {
        bn *sum = bn_add(l, r);
        if (sum == NULL) {
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            return BN_NO_MEMORY;
        }
        bn *m = bn_div(sum, two);
        if (m == NULL) {
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            bn_delete(sum);
            return BN_NO_MEMORY;
        }
        bn *pow = bn_init(m);
        if (pow == NULL) {
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            bn_delete(sum);
            bn_delete(m);
            return BN_NO_MEMORY;
        }
        code = bn_pow_to(pow, reciprocal);
        if (code != BN_OK) {
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            bn_delete(sum);
            bn_delete(m);
            bn_delete(pow);
            return code;
        }
        int cmp = bn_cmp(pow, t);
        bn_delete(pow);
        bn_delete(sum);
        if (cmp == 1) {
            code = bn_copy_to(r, m);
            if (code != BN_OK) {
                bn_delete(l);
                bn_delete(r);
                bn_delete(loopl);
                bn_delete(one);
                bn_delete(two);
                bn_delete(m);
                return code;
            }
        }
        else if (cmp == 0) {
            code = bn_copy_to(t, m);
            if (code != BN_OK) {
                bn_delete(t);
                bn_delete(l);
                bn_delete(r);
                bn_delete(loopl);
                bn_delete(one);
                bn_delete(two);
                bn_delete(m);
                return code;
            }
            bn_delete(m);
            bn_delete(one);
            bn_delete(two);
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            return BN_OK;
        }
        else {
            code = bn_copy_to(l, m);
            if (code != BN_OK) {
                bn_delete(l);
                bn_delete(r);
                bn_delete(loopl);
                bn_delete(one);
                bn_delete(two);
                bn_delete(m);
                return code;
            }
        }
        bn_delete(m);
        code = bn_copy_to(loopl, l);
        if (code != BN_OK) {
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            return code;
        }
        code = bn_add_to(loopl, one);
        if (code != BN_OK) {
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            return code;
        }
    }
    bn *powr = bn_init(r);
    if (powr == NULL) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        bn_delete(one);
        bn_delete(two);
        return BN_NO_MEMORY;
    }
    code = bn_pow_to(powr, reciprocal);
    if (code != BN_OK) {
        bn_delete(l);
        bn_delete(r);
        bn_delete(loopl);
        bn_delete(one);
        bn_delete(two);
        bn_delete(powr);
        return code;
    }
    if (bn_cmp(powr, t) == 1) {
        code = bn_copy_to(t, l);
        if (code != BN_OK) {
            bn_delete(t);
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            bn_delete(powr);
            return code;
        }
    }
    else {
        code = bn_copy_to(t, r);
        if (code != BN_OK) {
            bn_delete(t);
            bn_delete(l);
            bn_delete(r);
            bn_delete(loopl);
            bn_delete(one);
            bn_delete(two);
            bn_delete(powr);
            return code;
        }
    }
    bn_delete(powr);
    bn_delete(one);
    bn_delete(two);
    bn_delete(l);
    bn_delete(r);
    bn_delete(loopl);
    return BN_OK;
}

const char *bn_to_string(bn const *t, int radix_to) {
    if (t == NULL) return NULL;
    if (is_zero(t)) {
        char *out = malloc(2);
        if (out == NULL) return NULL;
        out[0] = DIGITS[0];
        out[1] = '\0';
        return (const char*)out;
    }
    bn *bn_copy = bn_init(t);
    if (bn_copy == NULL) return NULL;
    int int_rem = 0, *ptr_rem = &int_rem;
    char *out = malloc(1);
    if (out == NULL) {
        bn_delete(bn_copy);
        return NULL;
    }
    size_t out_cap = 1, *ptr_out_cap = &out_cap;
    size_t index = 0;
    while (bn_copy->body[0] > 0 || bn_copy->bodysize > 1) {
        int code = bn_div_short(bn_copy, radix_to, ptr_rem);
        if (code != BN_OK) {
            bn_delete(bn_copy);
            free(out);
            return NULL;
        }
        if (out_cap <= index) {
            out = expand_string(out, ptr_out_cap);
            if (out == NULL) {
                bn_delete(bn_copy);
                return NULL;
            }
        }
        out[index] = DIGITS[int_rem];
        index++;
    }
    if (bn_copy->sign) {
        out = realloc(out, index+1);
        if (out == NULL) {
            bn_delete(bn_copy);
            return NULL;
        }
        out[index] = '-';
        out = reverse(out, index+1);
        out = realloc(out, index+2);
        if (out == NULL) {
            bn_delete(bn_copy);
            return NULL;
        }
        out[index+1] = '\0';
    }
    else {
        out = reverse(out, index);
        out = realloc(out, index+1);
        if (out == NULL) {
            bn_delete(bn_copy);
            return NULL;
        }
        out[index] = '\0';
    }
    bn_delete(bn_copy); 
    const char* ret = (const char*) out;
    return ret;
}
