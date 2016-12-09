task ancestor
     :: message=(lang: en="hi" de="hallo")
     > out
{
	echo ${message} > ${out}
}

task foo
     < in=$out@ancestor[lang:en]
     > out
{
	cat < ${in} > ${out}
}

task bar
     < in=$out@foo[lang:de]
     > out
{
	cat < ${in} > ${out}
}

plan {
     # This should fail
     reach bar
}
