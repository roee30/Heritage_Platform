#!/bin/bash -e
cd "$(dirname $0)"
function tag() {
    echo "<$1 $3>$2</$1>"
}
exe=ML/interface
{
    QUERY_STRING="t=VH&text=$1" "$exe"
    tag style "$(cat style.css tooltip.css)"
} | tee out.html
