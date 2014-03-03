#ifndef UTIL_DATASTRUCTURES_H
#define UTIL_DATASTRUCTURES_H

#include <vector>
#include <map>
#include <algorithm>
#include <exception>

/*
 *  A collection of data structures missing in the standard library
 *   which we  require
 */

namespace util {

/*
 * A map which, unlike std::map, preserves the order of insertions
 * Internally, a vector is used
 */
template<typename Key, typename Value>
class InsertionOrderPreservingMap
{
  private:
    std::vector<std::pair<Key,Value>> mapping;

    auto find_pos(const Key & key) -> decltype(mapping.end()) {
      return std::find_if(
        mapping.begin(),
        mapping.end(),
        [&](std::pair<Key,Value> & pair) {
          return key == pair.first;
      });
    }
  public:
    auto begin() -> decltype(mapping.begin()) {
      return mapping.begin();
    }
    auto end() -> decltype(mapping.end()) {
      return mapping.end();
    }

    auto as_list() -> decltype(mapping) {return mapping;}

    auto operator[](const Key & key) -> decltype(std::map<Key, Value> {}.at(key)) {
      auto pos = find_pos(key);
      if (pos == mapping.end()) {
        auto new_elem = std::make_pair(key, Value {});
        mapping.push_back(new_elem);
        return (--mapping.end())->second;
      }
      return pos->second;
    }

    auto at(const Key & key) -> decltype(operator[](key)) {
      auto pos = find_pos(key);
      if (pos == mapping.end()) {
        throw std::out_of_range("Member not found!");
      }
      return *pos;
    }


    auto empty() -> decltype(mapping.empty()) {
      return mapping.empty();
    }

    auto insert(std::pair<Key,Value> elem) -> decltype(mapping.push_back(elem)) {
      mapping.push_back(elem);
    }

    auto find(const Key & key) -> decltype(mapping.end()) {
      return find_pos(key);
    }
};

}
#endif
