module main

fn sum_range(n int) int {
	mut total := 0
	for i in 0 .. n {
		total = total + i
	}
	return total
}

fn factorial(n int) int {
	mut result := 1
	for i in 1 .. n + 1 {
		result = result * i
	}
	return result
}

fn main() {
	// Sum 0..10 = 0+1+2+...+9 = 45
	println(sum_range(10))
	// 5! = 120
	println(factorial(5))
	// Sum 0..1 = 0
	println(sum_range(1))
	// 1! = 1
	println(factorial(1))
}
