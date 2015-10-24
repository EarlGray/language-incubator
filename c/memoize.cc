#include <iostream>
#include <functional>
#include <unordered_map>

template<class Arg, class Res>
class memoize {
    std::function<Res(Arg)> func;
    std::unordered_map<Arg, Res> mem;

public:
    explicit memoize(std::function<Res(Arg)> func): func{func}, mem{}
    {}

    Res operator ()(Arg arg) {
        auto it = mem.find(arg);
        if (it != mem.end()) {
            return it->second;
        }

        Res res = this->func(arg);
        this->mem[arg] = res;
        return res;
    }
};

int fib(int x) {
    if (x < 2) return 1;
    return fib(x - 1) + fib(x - 2);
}

int main() {
    auto memfib = memoize<int, int>(fib);
    std::cout << "memtwice(40) = " << memfib(40) << std::endl;
    std::cout << "memtwice(40) = " << memfib(40) << std::endl;
}
