#!/bin/bash

function usage {
cat << EOF
Usage: $0 m n
	m,n in Z
EOF
}

function gcd {
  local m n gcd temp

  m=$1
  n=$2
  gcd=1

  [ $m -eq $n ] && {
    echo $m
    return
  }

  while [ $n -ne 0 ]; do
    temp=$n
    n=$(($m % $n))
    m=$temp
  done

  echo $m
}

[ $# -ne 2 ] && {
  usage
  exit 1
}

GCD=$(gcd $1 $2)
echo $GCD
