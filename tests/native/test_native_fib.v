fn fib(n int) int {
	mut a := 0
	mut b := 1
	for i := 0; i < n; i++ {
		c := a + b
		a = b
		b = c
	}
	return a
}

fn main() {
	fib(10)
}
