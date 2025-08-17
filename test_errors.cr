// Test program with semantic errors
<int32> x = 42;
<float64> y = "hello";  // Type mismatch error

// Using undefined variable
<int32> z = undefined_var;

// Using undefined class
<UndefinedClass> obj;

class TestClass {
    private <int32> value;
    
    public <int32> getValue() {
        return value;
    }
}

public <int32> main() {
    <TestClass> test;
    <string> name = test->nonExistentField;  // Field doesn't exist
    <int32> result = test::nonExistentMethod();  // Method doesn't exist
    
    if x {  // Using int as boolean - should be error
        y = 123;
    }
    
    return "not an int";  // Return type mismatch
}
