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
     < in1=$out@ancestor[lang:de]
     < in2=$out@foo
     > out
{
	cat < ${in1}  > ${out}
	cat < ${in2} >> ${out}
}

plan {
     # This should succeed
     reach bar
}
