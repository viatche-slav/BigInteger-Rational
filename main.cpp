#include <iostream>
#include <vector>


enum class Sign {
  MINUS, ZERO, PLUS
};


class BigInteger;


BigInteger operator ""_bi(unsigned long long new_number);

BigInteger operator ""_bi(const char* new_number, uint64_t size);


bool operator==(const BigInteger& first, const BigInteger& second);

bool operator!=(const BigInteger& first, const BigInteger& second);

bool operator<(const BigInteger& first, const BigInteger& second);

bool operator>(const BigInteger& first, const BigInteger& second);

bool operator<=(const BigInteger& first, const BigInteger& second);

bool operator>=(const BigInteger& first, const BigInteger& second);


BigInteger operator+(const BigInteger& first, const BigInteger& second);

BigInteger operator-(const BigInteger& first, const BigInteger& second);

BigInteger operator*(const BigInteger& first, const BigInteger& second);

BigInteger operator/(const BigInteger& first, const BigInteger& second);

BigInteger operator%(const BigInteger& first, const BigInteger& second);


std::ostream& operator<<(std::ostream& out, const BigInteger& number);

std::istream& operator>>(std::istream& in, BigInteger& number);


class BigInteger {
 private:
  static const uint64_t base_ = 1000000000;
  Sign sign_;
  std::vector<uint64_t> digits_;

  void swap(BigInteger& other) {
    std::swap(sign_, other.sign_);
    std::swap(digits_, other.digits_);
  }

  static BigInteger subtraction(const BigInteger& f, const BigInteger& s) {
    BigInteger first = f, second = s;
    bool taken = false;
    for (uint64_t i = 0; i < second.digits_.size(); ++i) {
      if (taken) {
        if (first.digits_[i] >= second.digits_[i] + 1) {
          first.digits_[i] -= second.digits_[i] + 1;
          taken = false;
        } else {
          first.digits_[i] += base_ - second.digits_[i] - 1;
        }
      } else {
        if (first.digits_[i] >= second.digits_[i]) {
          first.digits_[i] -= second.digits_[i];
        } else {
          first.digits_[i] += base_ - second.digits_[i];
          taken = true;
        }
      }
    }
    if (taken) {
      for (uint64_t i = second.digits_.size(); i < first.digits_.size(); ++i) {
        if (first.digits_[i] >= 1) {
          first.digits_[i] -= 1;
          break;
        } else {
          first.digits_[i] = base_ - 1;
        }
      }
    }
    while (first.digits_.back() == 0) {
      if (first.digits_.size() == 1) {
        first.sign_ = Sign::ZERO;
        break;
      }
      first.digits_.pop_back();
    }
    return first;
  }

  BigInteger simpleMultiplication(uint64_t digit, uint64_t shift) const {
    BigInteger result;
    if (digit == 0 or sign_ == Sign::ZERO) return result;
    result.sign_ = Sign::PLUS;
    result.digits_.resize(shift, 0);
    uint64_t x = 0;
    for (uint64_t copy_digit : digits_) {
      x += digit * copy_digit;
      result.digits_.push_back(x % base_);
      x /= base_;
    }
    if (x != 0) result.digits_.push_back(x);
    return result;
  }

  static std::string digitToString(uint64_t digit) {
    std::string str = "000000000";
    int i = 8;
    while (digit > 0) {
      str[i] = char(int('0') + digit % 10);
      digit /= 10;
      --i;
    }
    return str;
  }

 public:
  friend BigInteger operator ""_bi(unsigned long long new_number);

  friend class Rational;

  explicit BigInteger(std::string str) : sign_(Sign::PLUS) {
    uint64_t index = 0;
    if (str[0] == '-') {
      ++index;
      if (index == str.size()) {
        sign_ = Sign::ZERO;
        digits_ = {0};
        return;
      } else {
        sign_ = Sign::MINUS;
      }
    }
    while (str[index] == '0') {
      ++index;
      if (index == str.size()) {
        sign_ = Sign::ZERO;
        digits_ = {0};
        return;
      }
    }
    uint64_t i = str.size();
    while (i >= 9 + index) {
      i -= 9;
      digits_.push_back(stoi(str.substr(i, 9)));
    }
    if (i > index) digits_.push_back(stoi(str.substr(index, i - index)));
  }

  BigInteger(int new_number) {
    if (new_number == 0) {
      sign_ = Sign::ZERO;
      digits_ = {0};
      return;
    }
    sign_ = (new_number > 0 ? Sign::PLUS : Sign::MINUS);
    new_number = abs(new_number);
    while (new_number > 0) {
      digits_.push_back(new_number % base_);
      new_number /= base_;
    }
  }

  BigInteger() : sign_(Sign::ZERO), digits_({0}) {}

  BigInteger& operator+=(const BigInteger& other) {
    if (other.sign() == Sign::ZERO) return *this;
    if (sign_ == Sign::ZERO) {
      sign_ = other.sign_;
      digits_ = other.digits_;
      return *this;
    }
    if (sign_ == other.sign_) {
      digits_.resize(std::max(digits_.size() + 1, other.digits_.size() + 1), 0);
      uint64_t i, x = 0;
      for (i = 0; i < other.digits_.size(); ++i) {
        x += digits_[i] + other.digits_[i];
        digits_[i] = x % base_;
        x /= base_;
      }
      while (x != 0) {
        digits_[i] = (x += digits_[i]) % base_;
        x /= base_;
        ++i;
      }
      if (digits_.back() == 0) digits_.pop_back();
      return *this;
    }
    return absIsLess(other) ? *this = subtraction(other, *this) : *this = subtraction(*this, other);
  }

  BigInteger& operator-=(const BigInteger& other) { return *this += -other; }

  BigInteger& operator*=(const BigInteger& other) {
    if (sign_ == Sign::ZERO) return *this;
    if (other.sign_ == Sign::ZERO) {
      sign_ = Sign::ZERO;
      digits_ = {0};
      return *this;
    }
    BigInteger result;
    uint64_t shift = 0;
    for (uint64_t digit : other.digits_) {
      result += simpleMultiplication(digit, shift);
      ++shift;
    }
    result.sign_ = (sign_ == other.sign_ ? Sign::PLUS : Sign::MINUS);
    swap(result);
    return *this;
  }

  BigInteger& operator/=(const BigInteger& other) {
    if (*this == other) {
      *this = 1;
      return *this;
    }
    if (absIsLess(other)) {
      sign_ = Sign::ZERO;
      digits_ = {0};
      return *this;
    }
    BigInteger dividend;
    dividend.sign_ = Sign::PLUS;
    dividend.digits_.resize(other.digits_.size());
    for (unsigned long i = 1; i <= other.digits_.size(); ++i) {
      dividend.digits_[dividend.digits_.size() - i] = digits_.back();
      digits_.pop_back();
    }
    if (dividend.absIsLess(other)) {
      dividend.digits_.insert(dividend.digits_.begin(), digits_.back());
      digits_.pop_back();
    }
    digits_.push_back(0);
    long long digit_position = digits_.size() - 1;
    while (digit_position != -1) {
      int left = 0, right = base_;
      while (left + 1 < right) {
        int middle = (left + right) / 2;
        dividend.absIsLess(other.simpleMultiplication(middle, 0)) ? right = middle : left = middle;
      }
      dividend -= other.simpleMultiplication(left, 0);
      digits_[digit_position] = left;
      while (dividend.absIsLess(other)) {
        if (--digit_position == -1) break;
        if (dividend.sign_ == Sign::ZERO) {
          dividend.digits_.pop_back();
          if (digits_[digit_position] != 0) dividend.sign_ = Sign::PLUS;
        }
        dividend.digits_.insert(dividend.digits_.begin(), digits_[digit_position]);
        digits_[digit_position] = 0;
      }
    }
    if (other.sign_ == Sign::MINUS) sign_ = (sign_ == Sign::PLUS ? Sign::MINUS : Sign::PLUS);
    return *this;
  }

  BigInteger& operator%=(const BigInteger& other) { return *this -= *this / other * other; }

  BigInteger operator+() const { return *this; }

  BigInteger operator-() const {
    BigInteger copy = *this;
    if (copy.sign_ != Sign::ZERO) { copy.sign_ = (copy.sign_ == Sign::PLUS ? Sign::MINUS : Sign::PLUS); }
    return copy;
  }

  BigInteger& operator++() { return *this += 1; }

  BigInteger operator++(int) {
    BigInteger copy = *this;
    *this += 1;
    return copy;
  }

  BigInteger& operator--() { return *this += -1; }

  BigInteger operator--(int) {
    BigInteger copy = *this;
    *this += -1;
    return copy;
  }

  explicit operator bool() const { return sign_ != Sign::ZERO; }

  Sign sign() const { return sign_; }

  uint64_t operator[](uint64_t index) const { return digits_[index]; }

  uint64_t size() const { return digits_.size(); }

  bool absIsLess(const BigInteger& other) const {
    if (digits_.size() < other.digits_.size()) return true;
    if (digits_.size() > other.digits_.size()) return false;
    for (unsigned long long i = 1; i <= digits_.size(); ++i) {
      if (digits_[digits_.size() - i] != other.digits_[other.digits_.size() - i])
        return (digits_[digits_.size() - i] < other.digits_[other.digits_.size() - i]);
    }
    return false;
  }

  std::string toString() const {
    std::string str;
    if (sign_ == Sign::MINUS) str += '-';
    str += std::to_string(*(digits_.end() - 1));
    for (uint64_t i = digits_.size() - 1; i >= 1; --i) {
      str += digitToString(digits_[i - 1]);
    }
    return str;
  }
};


BigInteger operator ""_bi(unsigned long long new_number) {
  BigInteger number;
  if (new_number == 0) { return number; }
  number.sign_ = Sign::PLUS;
  number.digits_.pop_back();
  while (new_number > 0) {
    number.digits_.push_back(new_number % BigInteger::base_);
    new_number /= BigInteger::base_;
  }
  return number;
}

BigInteger operator ""_bi(const char* new_number, uint64_t) {
  BigInteger number(new_number);
  return number;
}


bool operator==(const BigInteger& first, const BigInteger& second) {
  if (first.size() != second.size() || first.sign() != second.sign()) return false;
  for (uint64_t i = 0; i < first.size(); ++i) {
    if (first[i] != second[i]) return false;
  }
  return true;
}

bool operator!=(const BigInteger& first, const BigInteger& second) { return !(first == second); }

bool operator<(const BigInteger& first, const BigInteger& second) {
  if (first.sign() != second.sign()) return first.sign() < second.sign();
  return first.sign() != Sign::ZERO &&
         (first.sign() == Sign::PLUS ? first.absIsLess(second) : second.absIsLess(first));
}

bool operator>(const BigInteger& first, const BigInteger& second) { return second < first; }

bool operator<=(const BigInteger& first, const BigInteger& second) { return !(second < first); }

bool operator>=(const BigInteger& first, const BigInteger& second) { return !(first < second); }


BigInteger operator+(const BigInteger& first, const BigInteger& second) {
  BigInteger copy = first;
  copy += second;
  return copy;
}

BigInteger operator-(const BigInteger& first, const BigInteger& second) {
  BigInteger copy = first;
  copy -= second;
  return copy;
}

BigInteger operator*(const BigInteger& first, const BigInteger& second) {
  BigInteger copy = first;
  copy *= second;
  return copy;
}

BigInteger operator/(const BigInteger& first, const BigInteger& second) {
  BigInteger copy = first;
  copy /= second;
  return copy;
}

BigInteger operator%(const BigInteger& first, const BigInteger& second) {
  BigInteger copy = first;
  copy %= second;
  return copy;
}


std::ostream& operator<<(std::ostream& out, const BigInteger& number) {
  out << number.toString();
  return out;
}

std::istream& operator>>(std::istream& in, BigInteger& number) {
  std::string str;
  in >> str;
  BigInteger new_number(str);
  number = new_number;
  return in;
}


BigInteger GCD(BigInteger first, BigInteger second) {
  while (first != 0 && second != 0) {
    if (first >= second) {
      first %= second;
    } else {
      second %= first;
    }
  }
  if (first != 0) return first;
  return second;
}


class Rational;


bool operator==(const Rational& first, const Rational& second);

bool operator!=(const Rational& first, const Rational& second);

bool operator<(const Rational& first, const Rational& second);

bool operator>(const Rational& first, const Rational& second);

bool operator<=(const Rational& first, const Rational& second);

bool operator>=(const Rational& first, const Rational& second);


Rational operator+(const Rational& first, const Rational& second);

Rational operator-(const Rational& first, const Rational& second);

Rational operator*(const Rational& first, const Rational& second);

Rational operator/(const Rational& first, const Rational& second);


class Rational {
 private:
  Sign sign_;
  BigInteger numerator, denominator;

  void Simplify() {
    BigInteger gcd = GCD(numerator, denominator);
    numerator /= gcd;
    denominator /= gcd;
  }

 public:
  Rational() : sign_(Sign::ZERO), numerator(0), denominator(1) {}

  Rational(int number) {
    if (number == 0) {
      sign_ = Sign::ZERO;
      numerator = 0;
      denominator = 1;
      return;
    }
    sign_ = (number > 0 ? Sign::PLUS : Sign::MINUS);
    numerator = abs(number);
    denominator = 1;
  }

  Rational(const BigInteger& number) {
    sign_ = number.sign_;
    if (sign_ == Sign::ZERO) {
      numerator = 0;
    } else if (sign_ == Sign::MINUS) {
      numerator = -number;
    } else {
      numerator = number;
    }
    denominator = 1;
  }

  Rational operator+() const { return *this; }

  Rational operator-() const {
    Rational copy = *this;
    if (copy.sign_ != Sign::ZERO) copy.sign_ = (copy.sign_ == Sign::PLUS ? Sign::MINUS : Sign::PLUS);
    return copy;
  }

  Rational& operator+=(const Rational& other) {
    if (sign_ == Sign::ZERO) {
      *this = other;
      return *this;
    }
    if (other.sign_ == Sign::ZERO) return *this;
    if (sign_ == other.sign_) {
      numerator *= other.denominator;
      numerator += denominator * other.numerator;
      denominator *= other.denominator;
      Simplify();
      return *this;
    }
    numerator *= other.denominator;
    numerator -= denominator * other.numerator;
    denominator *= other.denominator;
    if (numerator.sign_ == Sign::ZERO) {
      sign_ = Sign::ZERO;
      denominator = 1;
      return *this;
    }
    if (numerator.sign_ == Sign::MINUS) {
      numerator.sign_ = Sign::PLUS;
      sign_ = (sign_ == Sign::PLUS ? Sign::MINUS : Sign::PLUS);
    }
    Simplify();
    return *this;
  }

  Rational& operator-=(const Rational& other) { return *this += -other; }

  Rational& operator*=(const Rational& other) {
    if (sign_ == Sign::ZERO) return *this;
    if (other.sign_ == Sign::ZERO) return *this = other;
    sign_ = (sign_ == other.sign_ ? Sign::PLUS : Sign::MINUS);
    numerator *= other.numerator;
    denominator *= other.denominator;
    Simplify();
    return *this;
  }

  Rational& operator/=(const Rational& other) {
    if (sign_ == Sign::ZERO) return *this;
    sign_ = (sign_ == other.sign_ ? Sign::PLUS : Sign::MINUS);
    numerator *= other.denominator;
    denominator *= other.numerator;
    Simplify();
    return *this;
  }

  explicit operator double() const { return std::stod(asDecimal(15)); }

  std::string toString() const {
    std::string number;
    if (sign_ == Sign::MINUS) number += "-";
    number += numerator.toString();
    if (denominator != 1) number += "/" + denominator.toString();
    return number;
  }

  std::string asDecimal(uint64_t precision = 0) const {
    std::string number;
    if (sign_ == Sign::ZERO) {
      number += '0';
      return number;
    }
    BigInteger bi_number = numerator;
    uint64_t i = 0;
    while (i + 9 <= precision) {
      bi_number *= BigInteger::base_;
      i += 9;
    }
    while (i < precision) {
      bi_number *= 10;
      i += 1;
    }
    bi_number /= denominator;
    number += bi_number.toString();
    if (number.size() <= precision) {
      std::string zeros(precision + 1 - number.size(), '0');
      number.insert(0, zeros);
    }
    if (precision > 0 && number != "0") number.insert(number.size() - precision, ".");
    if (sign_ == Sign::MINUS && number != "0") number.insert(0, "-");
    return number;
  }

  Sign sign() const { return sign_; }

  BigInteger Numerator() const { return numerator; }

  BigInteger Denominator() const { return denominator; }

  bool absIsLess(const Rational& other) const {
    return Numerator() * other.Denominator() < Denominator() * other.Numerator();
  }
};


bool operator==(const Rational& first, const Rational& second) {
  return first.sign() == second.sign() &&
         first.Numerator() == second.Numerator() &&
         first.Denominator() == second.Denominator();
}

bool operator!=(const Rational& first, const Rational& second) {
  return !(first == second);
}

bool operator<(const Rational& first, const Rational& second) {
  if (first.sign() != second.sign()) return first.sign() < second.sign();
  return first.sign() != Sign::ZERO &&
         (first.sign() == Sign::PLUS ? first.absIsLess(second) : second.absIsLess(first));
}

bool operator>(const Rational& first, const Rational& second) { return second < first; }

bool operator<=(const Rational& first, const Rational& second) { return !(second < first); }

bool operator>=(const Rational& first, const Rational& second) { return !(first < second); }


Rational operator+(const Rational& first, const Rational& second) {
  Rational copy = first;
  copy += second;
  return copy;
}

Rational operator-(const Rational& first, const Rational& second) {
  Rational copy = first;
  copy -= second;
  return copy;
}

Rational operator*(const Rational& first, const Rational& second) {
  Rational copy = first;
  copy *= second;
  return copy;
}

Rational operator/(const Rational& first, const Rational& second) {
  Rational copy = first;
  copy /= second;
  return copy;
}
