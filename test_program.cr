// Simple Cresta test program
<int32> x = 42;
<float64> result = 3.14;

class Calculator {
    private <float64> value;
    
    public <float64> add(<float64> a, <float64> b) {
        return a + b;
    }
    
    public void setValue(<float64> v) {
        value = v;
    }
}

public <int32> main() {
    <Calculator> calc;
    <float64> sum = calc::add(x, result);
    
    if sum > 40.0 {
        calc::setValue(sum);
    } or {
        calc::setValue(0.0);
    }
    
    return 0;
}
