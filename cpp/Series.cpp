#include <iostream>
#include <vector>

// Increasing range
class Range {
        double value;
  const double vstep;
  const double end;

public:
  using Point = double;

  Range(double start, double step, double end)
    : value(start), vstep(step), end(end) {}

  double getTime()      { return value; }
  double getNextTime()  { return value + vstep; }
  Point& getPoint()     { return value; }
  bool  done()          { return value > end; }
  void  step()          { if (!done()) value += vstep; }
};



// Data with a time-stamp
template <typename T>
struct TimePoint {
  double time;
  T      data;
};


// Record points when generated (like `tee`)
template <typename Series>
class Trace {
public:
  using Point = typename Series::Point;
  std::vector< TimePoint<Point> > trace;

private:
  Series& data;

  void snapshot() { trace.push_back({ data.getTime(), data.getPoint() }); }

public:
  Trace (Series &data) : data(data) { snapshot(); }
  double getTime()     { return data.getTime(); }
  double getNextTime() { return data.getNextTime (); }
  bool   done()        { return data.done(); }
  Point& getPoint()    { return data.getPoint(); }
  void step() {
    data.step();
    if (!done()) snapshot();
  }
};



// Filter `Data` on the points specified by `Time`
template <typename Time, typename Data>
class Sample {
  public:
    using Point = typename Data::Point;

  private:
    Time& time;
    Data& data;     // actual model
    Point point;    // point we are emitting

public:
  Sample (Time& time, Data& data) : time(time), data(data) { step(); }

  double getTime()        { return time.getTime(); }
  double getNextTime()    { return time.getNextTime(); }
  bool done()             { return time.done(); }

  Point& getPoint()       { return point; }

  void step() {
    if (done()) return;

    time.step();
    while (data.getNextTime() <= time.getTime()) data.step();
    point = data.getPoint();
  }
};


template <typename Data>
Trace<Data> trace(Data& data) { return Trace<Data>(data); }

template <typename Time, typename Data>
Sample<Time,Data> sample(Time &t, Data &d) { return Sample<Time,Data>(t,d); }

struct Example {
  using Point = double;

  double time;
  Point data;

  Example() : time(0), data(0) {}

  Point& getPoint()    { return data; }
  double getTime()     { return time; }
  double getNextTime() { return time + 5; }

  void step() {
    time += 5;
    data = 2 * time;
  }
};



int main() {
  Range time(0,7,120);
  Example example;
  auto sampled = sample<>(time,example);
  auto data    = trace<>(sampled);

  while (!data.done()) {
    std::cout << data.getPoint() << std::endl;
    data.step();
  }

  for (auto it = data.trace.begin(); it != data.trace.end(); ++it) {
    std::cout << it->data << " ";
  }
  std::cout << std::endl;
}

