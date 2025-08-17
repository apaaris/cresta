// Very simple method call test
class Point {
private:
    <int32> x;
public:
    <int32> get_value() {
        return 42;
    }
}

<int32> main() {
    <Point> p;
    p->get_value();
    return 0;
}
