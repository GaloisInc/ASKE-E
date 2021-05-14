#ifndef _ASKEE_HPP_
#define _ASKEE_HPP_

#include <iostream>
#include <vector>
#include <functional>

template<typename TModel>
class Model {
public:
  using Point = TModel;

  Model(TModel& m) : model(m) {
    next();
  }

  double getTime()      { return model.time; }
  TModel& getPoint()    { return model; }

  double getNextTime()  { return next_time; }
  bool  done()          { return false; }
  void  step() {
    model.run_event(next_event, next_time);
    next();
  }

  // XXX: does this need to be private?
  void next() {
    model.next_event(next_event, next_time);
  }

  void reset() {
    model.reset();
    next();
  }
private:

  TModel& model;
  double next_time;
  int next_event;
};

// Increasing range
class Range {
  double value;
  double vstep; // this...
  double end;   // ...and this were once `const`

public:
  using Point = double;

  Range(double start, double end, double step)
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
    Time time;
    Data& data;     // actual model
    Point point;    // point we are emitting

public:
  Sample (Time const& time, Data& data) : time(time), data(data) { step(); }

  double getTime()        { return time.getTime(); }
  double getNextTime()    { return time.getNextTime(); }
  bool done()             { return time.done(); }

  Point& getPoint()       { return point; }

  void step() {
    if (done()) return;

    time.step();
    while (data.getNextTime() <= time.getTime()) data.step();
    point = data.getPoint();
    // point.time = time.getTime(); XXX might want to bring this back
  }
};

template <typename Data>
Trace<Data> trace(Data& data) { return Trace<Data>(data); }

template <typename Time, typename Data>
Sample<Time,Data> sample(Time t, Data &d) { return Sample<Time,Data>(t,d); }

template<typename T>
T identity(T x) { return x; }

template<typename A, typename B, typename C>
std::function<C(A)> compose(std::function<C(B)> f, std::function<B(A)> g) {
  return [=](A x) { return f(g(x)); };
}

// A special wrapper for `Model`s
template<typename Data, typename Product = Data> // ew
class Random {
public:
  using Point = Product;

  Random(Model<Data>& m) : member(m), lens(identity<Data>), time_domain(Range{0,0,0}) {
    member.next();
  }

  Random(Model<Data>& m, std::function<Product(Data)> l) : member(m), lens(l), time_domain(Range{0,0,0}) {
    member.next();
  }

  double  getTime()           { return member.getTime(); }
  Product getPoint()    const { return lens(member.getPoint()); } // XXX is const correct?
  double  getNextTime()       { return member.getNextTime(); }
  bool    done()              { return member.done(); }
  void    step()              { member.step(); }

  template<typename NewProduct>
  Random<Data, NewProduct> select(std::function<NewProduct(Product)> f) {
    std::function<NewProduct(Data)> new_lens = compose(f, lens);
    Random<Data, NewProduct> new_random(member, new_lens);
    return new_random;
  }

  Random operator+(Random other) {
    std::function<Product(Product)> adder = [=](Product x){ return x + other.getPoint(); };
    Random<Data, Product> new_random(member, compose(adder, lens));
    return new_random;
  }
  Random operator*(Random& other) {
    std::function<Product(Product)> adder = [=](Product x){ return x * other.getPoint(); };
    Random<Data, Product> new_random(member, compose(adder, lens));
    return new_random;
  }
  // etc.

  Random at(Range r) {
    Random<Data, Product> new_random(member, lens);
    new_random.time_domain = r;
    return new_random;
  }
  Random at(double d) {
    Random<Data, Product> new_random(member, lens);
    new_random.time_domain = Range{d,d+1,1};
    return new_random;
  }

  std::vector<std::vector<Product>> make_samples(int n) {
    std::vector<std::vector<Product>> results;
    while (n --> 0) {
      std::vector<Product> result;
      auto sampler = sample<Range, Random<Data, Product>>(time_domain, *this);
      while (!sampler.done()) {
        result.push_back(sampler.getPoint());
        sampler.step();
      }
      results.push_back(result);
      member.reset();
    }
    return results;
  }

private:
  Model<Data>& member;
  std::function<Product(Data)> lens;
  Range time_domain;
  void* measure;
};

#endif