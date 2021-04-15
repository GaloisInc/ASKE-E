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


struct SIR {
  using Point = SIR;
  double time = 0;
  double s = 0;
  double i = 0;
  double r = 0;
  double  getTime() { return time; }
  double  getNextTime() { return time + 2; }
  Point&  getPoint() { return *this; }

  void step() {
    time = getNextTime();
    i = i + 1;
  }
};

template <typename T3>
struct Peak {
  double peak_value;
  double peak_time;
  Peak()
  : peak_value(0.0), peak_time(0.0)
  {}
  void addPoint(T3 &p) {
    if ((p.i >= peak_value)) {
      peak_value = p.i;
      peak_time = p.time;
    }
  }
};


// x = sample SIR() with [0..120 by 1]
// p = measure x with Peak()      <-    x :: SIR  (ElementOf Sir)
//                                      Peak<SIR>



/*

*/

struct X {
  Peak<SIR> p;

  void run() {
    Range xRange(0,1,120);
    SIR xModel;
    auto x = sample(xRange,xModel);
    p = {};

    while (!x.done()) {
      x.step();
      p.addPoint(x.getPoint());
    }
  }
};

int main() {
  X x;
  x.run();
  std::cout << x.p.peak_value << std::endl;

#if 0
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
#endif
}

