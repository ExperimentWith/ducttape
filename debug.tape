task foo
	:: param=(Letter: a b c d)
	> letter
{
    echo ${param} > ${letter}
}

task bar
	< in=$letter@foo[Letter:*]
{
	echo ${in}
}

