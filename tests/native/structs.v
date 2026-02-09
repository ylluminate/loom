module main

struct Point {
	x int
	y int
}

fn main() {
	// Struct creation
	p := Point{x: 10, y: 20}
	println(p.x)
	println(p.y)

	// Struct with computed values
	q := Point{x: p.x + 5, y: p.y * 2}
	println(q.x)
	println(q.y)
}
