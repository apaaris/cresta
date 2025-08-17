// Array example
<int32> main() {
    <int32[5]> numbers = [1, 2, 3, 4, 5];
    <int32> sum = 0;
    
    <int32> i = 0;
    while (i < 5) {
        sum = sum + numbers[i];
        i = i + 1;
    }
    
    return sum;
}
