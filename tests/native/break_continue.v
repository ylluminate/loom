module main

fn first_divisible_by_7(start int, limit int) int {
	for i in start .. limit {
		if i % 7 == 0 {
			return i
		}
	}
	return -1
}

fn sum_odd_only(n int) int {
	mut total := 0
	for i in 0 .. n {
		if i % 2 == 0 {
			continue
		}
		total = total + i
	}
	return total
}

fn sum_until_threshold(limit int, threshold int) int {
	mut total := 0
	for i in 0 .. limit {
		total = total + i
		if total >= threshold {
			break
		}
	}
	return total
}

fn main() {
	// First number >= 10 divisible by 7 is 14
	println(first_divisible_by_7(10, 100))
	// Sum of odd numbers 0..10: 1+3+5+7+9 = 25
	println(sum_odd_only(10))
	// Sum until >= 15: 0+1+2+3+4+5 = 15
	println(sum_until_threshold(100, 15))
}
