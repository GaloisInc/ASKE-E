#include <map>
#include <set>
 
template<typename Goal>
class topological_sorter {
protected:
    struct relations {
        std::size_t dependencies;
        std::set<Goal> dependents;
    };
    std::map<Goal, relations> map;
public:
    void add_goal(Goal const &goal) {
        map[goal];
    }
    void add_dependency(Goal const &goal, Goal const &dependency) {
        if (dependency == goal)
            return;
        auto &dependents = map[dependency].dependents;
        if (dependents.find(goal) == dependents.end()) {
            dependents.insert(goal);
            ++map[goal].dependencies;
        }
    }
    template<typename Container>
    void add_dependencies(Goal const &goal, Container const &dependencies) {
        for (auto const &dependency : dependencies)
            add_dependency(goal, dependency);
    }
    template<typename ResultContainer, typename CyclicContainer>
    void destructive_sort(ResultContainer &sorted, CyclicContainer &unsortable) {
        sorted.clear();
        unsortable.clear();
        for (auto const &lookup : map) {
            auto const &goal = lookup.first;
            auto const &relations = lookup.second;
            if (relations.dependencies == 0)
                sorted.push_back(goal);
        }
        for (std::size_t index = 0; index < sorted.size(); ++index)
            for (auto const &goal : map[sorted[index]].dependents)
                if (--map[goal].dependencies == 0)
                    sorted.push_back(goal);
        for (auto const &lookup : map) {
            auto const &goal = lookup.first;
            auto const &relations = lookup.second;
            if (relations.dependencies != 0)
                unsortable.push_back(goal);
        }
    }
    template<typename ResultContainer, typename CyclicContainer>
    void sort(ResultContainer &sorted, CyclicContainer &unsortable) {
        topological_sorter<Goal> temporary = *this;
        temporary.destructive_sort(sorted, unsortable);
    }
    void clear() {
        map.clear();
    }
};
