task foo
    :: param=(Letter: a b c d e f g h i)
     > letter
{
    echo ${param} > ${letter}
}

task bar
    < letters=$letter@foo[Letter:*]
{
    echo ${letters}
}
