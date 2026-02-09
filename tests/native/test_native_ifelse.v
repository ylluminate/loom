fn abs_val(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

fn max_val(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn fizzbuzz_count(n int) int {
	mut count := 0
	for i := 1; i <= n; i++ {
		if i % 15 == 0 {
			count += 1
		} else if i % 3 == 0 {
			count += 1
		} else if i % 5 == 0 {
			count += 1
		}
	}
	return count
}

fn main() {
	println(abs_val(-42))
	println(max_val(10, 20))
	println(fizzbuzz_count(100))
	println(0)
	println(-123)
}
