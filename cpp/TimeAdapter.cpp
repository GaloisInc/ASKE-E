#include <iostream>

template <typename Model>
class TimeAdapter {
  const double time_start;
  const double time_step;
  const double time_end;

  Model& model;           // actual model
  Model  point;           // point we are emitting
  double point_time;      // current time

public:
  TimeAdapter(double start, double step, double end, Model& model)
    : time_start(start), time_step(step), time_end(end),
      model(model), point(model)
  {
    this->step();
  }

  Model& getPoint() { return point; }
  double getTime() { return point_time; }

  bool done() { return point_time > time_end; }

  void step() {
    if(done())
      return;
    while(model.getNextTime() <= point_time + time_step)
      model.step();

    point = model;
    point_time = point_time + time_step;
  }
};

template<typename TModel, typename TPointType, typename TMeasure>
class WithMeasure {
public:
  WithMeasure(TModel& model, TMeasure& measure)
    : measure { measure }, model { model }
  {
    doMeasure();
  }

  bool done() {
    return model.done();
  }

  double getTime() {
    return model.getTime();
  }

  TPointType& getPoint() {
    return model.getPoint();
  }

  void step() {
    if(!model.done()) {
      model.step();
      doMeasure();
    }
  }

private:
  void doMeasure() {
    measure.withPoint(model.getPoint());
  }

  TMeasure& measure;
  TModel& model;
};


struct Example {
  double time;
  double data;

  Example() : time(0), data(0) {}

  double getTime() { return time; }

  double getNextTime() { return time + 5; }

  void step() {
    time += 5;
    data = 2 * time;
  }

};

class SumMeasure {
public:
  void withPoint(Example const& p) {
    value += p.data;
  }

  double value = 0.0;
};

template<typename TModel, typename TPointType, typename TMeasure>
WithMeasure<TModel, TPointType, TMeasure> withMeasure(TModel& m, TMeasure t) {
  return {m, t};
}

int main() {
  Example model;
  TimeAdapter<Example> timed(0,1,120,model);
  SumMeasure sum;
  // template trickery might make this a little more palatable
  auto timed_measured =
    WithMeasure<TimeAdapter<Example>, Example, SumMeasure> { timed, sum };

  while (!timed_measured.done()) {
    Example &m = timed_measured.getPoint();
    std::cout << "time = " << timed_measured.getTime() << ", data = " << m.data << std::endl;
    timed_measured.step();
  }

  std::cout << "sum = " << sum.value << std::endl;
}

