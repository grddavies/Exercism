#include "triangle.h"
#include <algorithm>
#include <array>

namespace triangle
{
  flavor kind(const double a, const double b, const double c)
  {
    std::array<double, 3> sides = {a, b, c};
    std::sort(sides.begin(), sides.end());

    if (sides[0] <= 0)
    {
      throw std::domain_error("All triangle sides lengths must be greater than 0");
    };

    if (sides[0] + sides[1] < sides[2])
    {
      throw std::domain_error("Violates the triangle inequality");
    }

    // Test 3 side equality
    if (sides[0] == sides[2])
    {
      return flavor::equilateral;
    };

    // Test 2 side equality
    if (sides[0] == sides[1] || sides[1] == sides[2])
    {
      return flavor::isosceles;
    }

    return flavor::scalene;
  }
} // namespace
