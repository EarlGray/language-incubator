/*
   * see: http://matt.might.net/articles/c++-template-meta-programming-with-lambda-calculus/
   */

#include <iostream>
#include <cassert>

using std::endl; 
using std::cout;

/*
 * Example of template specialization
 */
template <typename A>
struct ListNode {
    A data;
    ListNode<A> *next;
};

template<>
struct ListNode<unsigned int>
{
    int data;
    ListNode<int> *next;
};


/*
 *  The mandatory example: factorial
 */
template<int N>
struct Factorial
{ 
    enum { value = N * Factorial<N-1>::value };
};

template<>
struct Factorial<0>
{
    enum { value = 1 };
};

/*
 *  Peano Numbers
 */
struct Zero { 
    enum { value = 0 };
};

template<typename N> 
struct Succ {
    enum { value = N::value + 1 } ;
};

typedef Succ<Zero> One;
typedef Succ<Succ<Zero> > Two;
typedef Succ<Two> Three;

template<typename N>
struct MatchOne {
    enum { value = 0 };
};

template<>
struct MatchOne<Succ<Zero> > {
    enum { value = 1 };
};

/* empty environment */
struct EmptyEnv;

template<int Name, typename Value, typename Env>
struct Binding {};

template<int Name, typename Env>
struct EnvLookup {};

template<int Name>
struct EnvLookup<Name, EmptyEnv> {}; // name not found

template <int Name, typename Value, typename Env>
struct EnvLookup<Name, Binding<Name, Value, Env> > {
    Value typedef result;
};

template <int Name, int Name2, typename Value2, typename Env>
struct EnvLookup<Name, Binding<Name2, Value2, Env> > {
    typename EnvLookup<Name, Env> :: result typedef result;
};

/*
 * Core Lambda Calculus syntax
 */
template<int FormalName, typename Body>
struct Lambda {};

template<typename Fun, typename Arg>
struct App {};

template<int Name>
struct Ref {};

// sugar
template<typename Cond, typename Then, typename Else>
struct If {};

template<typename T>
struct Lit {};

// Values
template<typename Lam, typename Env>
struct Closure {};

struct True {};
struct False {};

/*
 * Eval/Apply
 */
template<typename Exp, typename Env>
struct Eval {};

template<typename Proc, typename Env>
struct Apply {};

template<typename T, typename Env>
struct Eval<Lit<T>, Env> {
    T typedef result;
};

template<int Name, typename Env>
struct Eval<Ref<Name>, Env> {
    typename EnvLookup<Name, Env> :: result typedef result;
};

template<int Name, typename Body, typename Env>
struct Eval<Lambda<Name, Body>, Env> {
    Closure<Lambda<Name, Body>, Env> typedef result;
};

template<typename Fun, typename Arg, typename Env>
struct Eval<App<Fun, Arg>, Env> {
    typename Apply<typename Eval<Fun, Env> :: result,
                   typename Eval<Arg, Env> :: result >
                       :: result typedef result;
};

// Branch true
template<typename Then, typename Else, typename Env>
struct Eval<If<True, Then, Else>, Env> {
    typename Eval<Then, Env> :: result typedef result;
};

template<typename Then, typename Else, typename Env>
struct Eval<If<False, Then, Else>, Env> {
    typename Eval<Else, Env> :: result typedef result;
};

// evaluate the condition
template<typename Cond, typename Then, typename  Else, typename Env>
struct Eval<If<Cond, Then, Else>, Env> {
    typename Eval<If<typename Eval<Cond, Env> :: result, Then, Else>,
                    Env> :: result typedef result;
};

template<int Name, typename Body, typename Env, typename Value>
struct Apply<Closure<Lambda<Name, Body>, Env>, Value> {
    typename Eval<Body, Binding<Name, Value, Env> > :: result
        typedef result;
};

int main() {
    cout << Factorial<7>::value << endl;
    cout << "Match 0: " << MatchOne<Zero>::value << endl;
    cout << "Match 1: " << MatchOne<Succ<Zero> >::value << endl;
    cout << "Match 2: " << MatchOne<Succ<Succ<Zero> > >::value << endl;

    /* testing the lambda calculus */
    // [2 => 1, 3 => 0](3)
    int v = EnvLookup<3, Binding<2, Succ<Zero>,
                         Binding<3, Zero,
                         EmptyEnv> > > :: result :: value;
    assert(v == 0);


    // ((lambda (x) x) 2);
    enum { X };
    int x = Eval<App<Lambda<X, Ref<X> >, Lit<Succ<Succ<Zero> > > >, EmptyEnv>::result::value;
    assert(x == 2);

    // (if #f 0 1)
    int y = Eval<If<Lit<False>, Lit<Zero>, Lit<Succ<Zero> > >, EmptyEnv>::result::value;
    assert(y == 1);

}
