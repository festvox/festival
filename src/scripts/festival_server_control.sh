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
#                                                                         #
# Run a festival server.                                                  #
#                                                                         #
###########################################################################

TOP=__TOP__
EST=__EST__

#__SHARED_SETUP__

useage()
{
cat <<EOF

Useage: festival_server_control [-p PORT] [-l LOGDIR] restart|exit|quit

Control a festival server.
    
	-p PORT		Port server is listening on, defaults to 1314

	-l LOGDIR	Server put it's logfile into LOGDIR, defaults to '.'

	restart		Restart the server

	exit		Terminate the server

	quit		Terminate the server
EOF

exit $1
}

port=1314
logdir="."
action=""

while [ $# -gt 0 ]
    do
    case "$1" in
    -h* | -\? ) useage 0;;
    -p* )	port=$2; shift 2;;
    -l* )	logdir=$2; shift 2;;
    -* )	useage 1;;
    * )		break;;
    esac
done

case ${1-''} in
restart|quit|exit )	action=$1;;
* )	useage 2;;
esac

if [ $port = "1314" ]
    then
    pext=""
else
    pext=".$port"
fi

wrapper_pid_file="$logdir/festival_wrapper_pid$pext"
sleep_pid_file="$logdir/festival_sleep_pid$pext"

if [ -f $wrapper_pid_file -a -f $sleep_pid_file ]
    then
    :
else
    echo "No festival server log information in $logdir"
    exit 1
fi

cat $wrapper_pid_file $sleep_pid_file |\
{
    read wrapper_pid
    read sleep_pid

    if kill -0 $wrapper_pid 2>/dev/null
	then
	case "$action" in
	restart ) kill -HUP $wrapper_pid;;
	kill|quit|exit ) kill -TERM $wrapper_pid;;
	esac
	kill -TERM $sleep_pid
    else
	echo "Server wrapper not running for port $port"
    fi

} 

exit 0

