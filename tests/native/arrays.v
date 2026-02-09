module main

fn main() {
	// Array creation with initial elements
	mut arr := [10, 20, 30]

	// Array indexing
	println(arr[0])
	println(arr[1])
	println(arr[2])

	// Array length
	println(arr.len)

	// Array append
	arr << 40
	println(arr[3])
	println(arr.len)

	// Array index assignment
	arr[1] = 99
	println(arr[1])
}
