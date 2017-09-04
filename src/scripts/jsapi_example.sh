#!/bin/sh
###########################################################################
##                                                                       ##
##                Centre for Speech Technology Research                  ##
##                     University of Edinburgh, UK                       ##
##                         Copyright (c) 1996                            ##
##                        All Rights Reserved.                           ##
##                                                                       ##
##  Permission is hereby granted, free of charge, to use and distribute  ##
##  this software and its documentation without restriction, including   ##
##  without limitation the rights to use, copy, modify, merge, publish,  ##
##  distribute, sublicense, and/or sell copies of this work, and to      ##
##  permit persons to whom this work is furnished to do so, subject to   ##
##  the following conditions:                                            ##
##   1. The code must retain the above copyright notice, this list of    ##
##      conditions and the following disclaimer.                         ##
##   2. Any modifications must be clearly marked as such.                ##
##   3. Original authors' names are not deleted.                         ##
##   4. The authors' names are not used to endorse or promote products   ##
##      derived from this software without specific prior written        ##
##      permission.                                                      ##
##                                                                       ##
##  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ##
##  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ##
##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ##
##  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
##  THIS SOFTWARE.                                                       ##
##                                                                       ##
###########################################################################
TOP=__TOP__
EST=__EST__
CLASSPATH=__CLASSPATH__
JAVA_HOME=__JAVA_HOME__

#__SHARED_SETUP__

useage() {

    cat <<EOF
    Useage jsapi_example [--server HOST] [--port NNNN] [-r CLASS] [-v] [-l] [-s] [TEXT...]

	--server HOST	Host wher efestival is running as a server. Can be
			hostname:port.

	--port NNNN	Port number at which festival is listening. 

	-r CLASS	Register CLASS as an EngineCentral with JSAPI.

	-l		List available synthesizers.

	-s		(Synchronous) Wait for each text before sending next.

	-v		Be more verbose.

	TEXT		Piece of text to be synthesized. Each argument is
			queued separately.

EOF
    exit $1;
}

parse_server() {
    p=`expr "$server" : '.*:\([0-9]*\)$'`
    if [ -n "$p" ]
	then
	port="$p"
	server=`expr "$server" : '^\([^:]*\):'`
    fi
}

server="$FESTIVAL_SERVER"
parse_server "$server"

while true
    do
    case "$1" in 
    -g )		g="_g -debug"; shift;;
    --server | -s )	server="$2"; parse_server "$server"; shift 2;;
    --port | -p )	port="$2"; shift 2;;
    -\? | --help )	useage 0;;
    * )			break;;
    esac
done

if [ -z "$server" ] || [ -z "$port" ]
    then
    useage 1
fi

CLASSPATH="$TOP/src/lib/festival.jar:$EST/lib/est___JAVA_VERSION__.jar:$CLASSPATH"

export CLASSPATH

exec $JAVA_HOME/bin/java$g \
    -Dfestival.server="$server" \
    -Dfestival.port="$port" \
    cstr.testPrograms.SayHelloWorld \
    "$@"





