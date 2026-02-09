module main

fn classify(n int) int {
	if n > 0 {
		return 1
	} else if n < 0 {
		return -1
	} else {
		return 0
	}
}

fn max(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn main() {
	println(classify(42))
	println(classify(-7))
	println(classify(0))
	println(max(10, 20))
	println(max(99, 3))
}
