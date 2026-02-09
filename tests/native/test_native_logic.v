fn clamp(x int, lo int, hi int) int {
	if x >= lo && x <= hi {
		return x
	}
	if x < lo {
		return lo
	}
	return hi
}

fn in_range(x int) int {
	// Returns 1 if x is negative OR x > 100
	if x < 0 || x > 100 {
		return 1
	}
	return 0
}

fn compound_ops(n int) int {
	mut x := n
	x *= 3
	x /= 2
	x %= 7
	return x
}

fn complex_cond(a int, b int, c int) int {
	// (a > 0 && b > 0) || c > 0
	if (a > 0 && b > 0) || c > 0 {
		return 1
	}
	return 0
}

fn main() {
	println(clamp(5, 1, 10))
	println(clamp(-3, 1, 10))
	println(clamp(20, 1, 10))
	println(in_range(-5))
	println(in_range(50))
	println(in_range(200))
	println(compound_ops(100))
	println(complex_cond(1, 1, 0))
	println(complex_cond(0, 0, 1))
	println(complex_cond(0, 0, 0))
}
