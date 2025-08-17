// Class example - C++ style
class Point {
private:
    <int32> x;
    <int32> y;

public:
    void set_position(<int32> new_x, <int32> new_y) {
        x = new_x;
        y = new_y;
    }
    
    <int32> get_x() {
        return x;
    }
    
    <int32> get_y() {
        return y;
    }
}

<int32> main() {
    <Point> p;
    p->set_position(5, 10);
    <int32> result = p::get_x() + p::get_y();
    return result;
}
