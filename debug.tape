global {
	foo="bar"
}

task parent1 :: z="zazz" {}

task parent2 :: q=(Q: q1="qiikw" q2="qikmiq" q3=(Sky: blue red=$z@parent1)) {}

task hello :: a="abc" b=$foo c=$z@parent1 myq=$q@parent2 > out {
	echo "Hello" > ${out}
}

plan {

	reach hello via (Q: *) * (Sky: *)

}
