// Multi-field class test
class Point {
private:
    <int32> x;
    <int32> y;

public:
    <int32> get_constant() {
        return 42;
    }
}

<int32> main() {
    <Point> p;
    return 0;
}
