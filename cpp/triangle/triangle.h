#if !defined(TRIANGLE_H)
#define TRIANGLE_H

namespace triangle
{
    enum class flavor
    {
        scalene,
        isosceles,
        equilateral,
    };

    flavor kind(const double a, const double b, const double c);
} // namespace triangle

#endif // TRIANGLE_H