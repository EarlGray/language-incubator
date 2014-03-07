define double @main(double %x) {
entry:
    %0 = alloca double
    br body

body:
    store double %x, double* %0
    %1 = load double* %0
    %2 = fadd double %1, 1.000000e+00
    ret double %2
}
