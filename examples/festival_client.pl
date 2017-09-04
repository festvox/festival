#!/usr/local/bin/perl

# festival_client.pl - a perl socket client for festival
#
# Copyright (C) 1997
# Kevin A. Lenzo (lenzo@cs.cmu.edu) 7/97
# All rights reserved.
#
# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
#
#########################################################################
#
# A good deal of this was taken from the example in the
# perlipc man page.  The rest is simply the Festival-specific 
# stuff. 

use IO::Socket;

package Festival;

my ($audio_command, $audio_file, $file_stuff_key);
my ($host, $port, $kidpid, $handle, $line);

$wave_type = "nist";                 # the type of the audio files
$audio_command  = "na_play";         # your local audio play command

# uncomment the next line if your audio command requires a file
# $audio_file = "/tmp/client_tmp$$.nist"; # temp file for waveforms

$file_stuff_key = "ft_StUfF_key";    # defined in speech tools

$host = shift || 'localhost';
$port = shift || 1314;
if ($host eq "-h") {
print STDOUT "
  Usage:

     $0 [<server> [<port>]]

         OR

     $0 -h 

  perl client for the Festival text-to-speech server

  Lines that do not begin with a ( are treated as text
  to be spoken.  Those that do begin with a ( are assumed
  to Scheme commands to festival and are sent without
  alteration.  Use `quit' or `exit' to quit.

  Note that a server must be started before the client
  will work.

  Use the `-h' (help) option for this message.

";

    exit(1);
}

# create a tcp connection to the specified host and port

$handle = IO::Socket::INET->new(Proto     => "tcp",
				PeerAddr  => $host,
				PeerPort  => $port)
    or die "
  Can't connect to port $port on $host: $!
  (Are you sure the server is running and accepting connections?)

";

$handle->autoflush(1);     # so output gets there right away
print STDERR "[Connected to $host:$port]\n";

# tell the server to send us back a 'file' of the right type
print $handle "(Parameter.set 'Wavefiletype '$wave_type)\n";

# split the program into two processes, identical twins
die "can't fork: $!" unless defined($kidpid = fork());

# the if{} block runs only in the parent process
if ($kidpid) {
    # the parent handles the input so it can exit on quit

    while (defined ($line = <STDIN>)) {
	last if ($line =~ /^(quit|exit)$/);

	if ($line =~ /^\(/) {
	    # just send it wholesale if it's a ( )
	    print $handle $line;
	} else {
	    # otherwise assume it's text to be spoken
	    chomp $line;
	    print $handle "(tts_textall \"$line\" 'file)\n";
	}
    }
    kill("TERM", $kidpid);# send SIGTERM to child
}
# the else{} block runs only in the child process
else {
    # the child is forked off to get the results from the server
    undef $line;
    while (($line = $remains) || defined ($line = <$handle>)) {
	undef $remains;
	if ($line eq "WV\n") { # we have a waveform coming
	    undef $result;
	    if ($audio_file) {
		open(AUDIO, ">$audio_file");
	    } else {
		open(AUDIO, "| $audio_command");
	    }
	    while ($line = <$handle>) {
		if ($line =~ s/$file_stuff_key(.*)$//s) {
		    $remains = $1;
		    print AUDIO $line;
		    last;
		}
		print AUDIO $line;
	    }
	    close AUDIO;

	    if ($audio_file) {
		# call the command if we weren't piping
		system("$audio_command $audio_file");
	    
		# remove the evidence
		unlink($audio_file);
            }
	} elsif ($line eq "LP\n") {
	    while ($line = <$handle>) {
		if ($line =~ s/$file_stuff_key(.*)$//s) {
		    $remains = $1;
		    print STDOUT $line;
		    last;
		}
		print STDOUT $line;
	    }
	} else {
	    # if we don't recognize it, echo it
	    print STDOUT $line;
	}
    }	 
}
