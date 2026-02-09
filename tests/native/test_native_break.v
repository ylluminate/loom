fn sum_until_break(n int) int {
	mut total := 0
	mut i := 1
	for {
		if i > n {
			break
		}
		total += i
		i += 1
	}
	return total
}

fn sum_odds(n int) int {
	mut total := 0
	for i := 1; i <= n; i++ {
		if i % 2 == 0 {
			continue
		}
		total += i
	}
	return total
}

fn collatz_steps(start int) int {
	mut steps := 0
	mut x := start
	for x != 1 {
		if x % 2 == 0 {
			x = x / 2
		} else {
			x = x * 3 + 1
		}
		steps += 1
	}
	return steps
}

fn main() {
	println(sum_until_break(10))
	println(sum_odds(10))
	println(collatz_steps(27))
}
