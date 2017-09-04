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

Useage: festival_server [-p PORT] [-l LOGDIR] [-show] [FESTIVAL]

Run festival as a server
    
	-p PORT		Port to listen on, defaults to 1314

	-l LOGDIR	Record what is happening in the given directory

	-c CONFIG	File of Scheme code to be loaded by festival
			to set access control and preload voices.

        -show		Print the configuration file which would have
			been used then exit.

	FESTIVAL	Festival itself if it isn't on the PATH.

If no CONFIG is given a default will be created. This is very minimal and
you should certainly create one which better reflects your needs.

Use the -show argument to get the default CONFIG file as something to work
from.

EOF

exit $1
}

log_mess()
{
date=`date`
echo "wrapper	$date : $@" >&3
}

handle_hup()
{
log_mess "got SIGHUP"
respawn_wait=0;
if [ $festival_pid = 0 ]
    then
    :
else
    kill -9 $festival_pid
    festival_pid=0
fi
}

handle_term()
{
log_mess "got SIGTERM"
respawn=false;
respawn_wait=0;
if [ $festival_pid = 0 ]
    then
    :
else
    kill -9 $festival_pid
    festival_pid=0
fi
}

create_server_startup()
{
    local port server_log su
    port="$1"
    server_log="$2"

    server_startup="$3"

    {
    cat <<EOF
(print "Load server start $server_startup")
(set! server_port $port)
(set! server_festival_version "$festival" )
(set! server_log_file "$server_log" )
(set! server_startup_file "$server_startup" )

;; Marks end of machine created bit
;---

EOF

    if [ -n "$config" ]
	then
	cat $config
    else
	log_mess "USING DEFAULT CONFIGURATION"
	hostname=`hostname`
	cat <<EOF

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; The default information below was created by the festival_server script
 ;; You should probably create a file which is similar but with whatever
 ;; access permissions and preloaded voices suit your local situation and
 ;; use the -c flag to festival_server

(defvar server_home "$logdir")

;; Access from machines with no domain name and the local 
(set! server_access_list '("[^.]+" "127.0.0.1" "localhost.*" "$hostname"))

(cd server_home)
(set! default_access_strategy 'direct)

;; Load any voices you regularly use here, this will make the
;; server more responsive

; (voice_rab_diphone)
; (voice_gsw_diphone )
; (voice_ked_diphone)
; (voice_ked_mttilt_diphone)

EOF
    fi
    } > $server_startup
}

port=1314
festival=festival
logdir="."
config=""
show=false

while [ $# -gt 0 ]
    do
    case "$1" in
    -h* | -\? ) useage 0;;
    -p* )	port=$2; shift 2;;
    -l* )	logdir=$2; shift 2;;
    -c* )	config=$2; shift 2;;
    -show* )	show=true; shift;;
    -* )	useage 1;;
    * )		break;;
    esac
done

case $# in
1 )	festival=$1;;
0 )	: ;;
* )	useage 2;;
esac

respawn=true
normal_respawn_wait=10
festival_pid=0

if [ $port = "1314" ]
    then
    pext=""
else
    pext=".$port"
fi

server_log="$logdir/festival_server$pext.log"

# hangup
trap "handle_hup" 1
# int
trap "handle_term" 2
# term (ie default kill signal)
trap "handle_term" 15
# exit from shell
trap "handle_term" 0

if $show
    then
    create_server_startup $port $server_log /tmp/$$ 3>/dev/null
    fl=false
    while read l
	do
	if $fl ; then echo $l ; fi
	if [ "$l" = ";---" ] ; then fl=true ; fi
    done </tmp/$$ 
    /bin/rm -f /tmp/$$
    exit 0
fi

if [ -f "$server_log" ]
    then
    date=`date|tr ' ' '_'`
    mv $server_log "$server_log.$date"
fi

{
echo $$ > "$logdir/festival_wrapper_pid$pext"

while $respawn
    do
    respawn_wait=$normal_respawn_wait

    if [ $festival_pid = 0 ]
	then
	:
    else
	kill -9 $festival_pid
        festival_pid=0
    fi

    log_mess "run $festival port=$port"

    create_server_startup $port $server_log "$logdir/festival_server$pext.scm"

    $festival --server $server_startup &

    x=$!
    if [ -n "$x" ]
	then
        festival_pid="$x"

	log_mess "waiting"
	
	while [ $festival_pid -ne 0 ] && kill -0 $festival_pid
	    do
	    sleep 60&
	    echo $! > "$logdir/festival_sleep_pid$pext"
	    wait $!
	done

	if [ "$festival_pid" = 0 ]
	    then
	    :
	else
	    wait $festival_pid
	fi

	log_mess "festival exited $?"
    else
	log_mess "can't run $festival"
    fi

    if $respawn
	then
	log_mess "respawn wait $respawn_wait seconds"
	sleep $respawn_wait
    fi
done
} >$server_log 3>$server_log

exit 0
