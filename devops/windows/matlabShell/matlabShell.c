/* */
/*
 *	matlabShell.c
 *
 *	This is a simple program call MATLAB matlab.el
 *
 *    Copyright (c) 1998 by Robert A. Morris
 *    Modified by Mikhail L. Titov 2012
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * This program is a matlab shell which will run under Windows XP+
 * and can be invoked from from matlab.el BUT NOT a native
 * command shell to get multiline commands support.
 *
 * See the usage notes at the end of this file, or invoke it with
 * -h argument for brief usage message

 *      01jun12 version 1.1 https://github.com/mlt/matlabShell
 *
 *      Known deficiencies in 1.1:
 *      1. Matlab standard command window pops up when starting
 */
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <Windows.h>
#include "engine.h"

#ifndef LANG_SYSTEM_DEFAULT /* lcc */
#define LANG_SYSTEM_DEFAULT MAKELANGID(LANG_NEUTRAL, SUBLANG_SYS_DEFAULT)
#endif /* LANG_SYSTEM_DEFAULT */

char want_quit = 0;

void sighandler(int s) {
  printf("\nCaught signal %d\n", s);
  want_quit = 1;
}

void printErr() {
  DWORD err = GetLastError();
  WCHAR buffer[1024];
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, err, LANG_SYSTEM_DEFAULT, (LPSTR)buffer, 1024, NULL);
  wprintf(buffer);
}

#define MAXLEN 10240;		/* default */
int main(int argc, char **argv)
{
  char version[]="MatlabShell 1.1. 01jun2012.\nCopyright 1998 Robert A. Morris.\nModified by Mikhail L. Titov 2012\nThis is Free Software licensed under the GNU Public License.";
	Engine *ep;
	int inputMax; /* buffer size */
	char *inbuf;

	int outputMax; /*buffer size */
	char *fromEngine;

	int noArgs;
	int len, pos;

	int debug = 0;
	int retval; /* for debug */

	HANDLE hStdIn = GetStdHandle(STD_INPUT_HANDLE);
	DWORD dwRead;
	DWORD dwRes;

	signal(SIGBREAK, sighandler); /* sent by FAR Manager when closed with [x] */
	signal(SIGINT, sighandler);   /* C-c */

	/* matlab.el always invokes the shell command with two
	   arguments, the second of which is NULL unless the lisp
	   variable matlab-shell-command-switches is set, in which
	   case the string value of that variable is passed as the
	   argv[1] to the shell program. In that case we have to parse
	   this string. In the standalone case, we may see 1, 2, or 3
	   args, the first of which is always the program path
	*/

	printf("%s\n", version);
	noArgs = (argc==1) || (argv[1]==0 || argv[1][0] == 0);

	if ( (!noArgs) && (argv[1][0] == '?' || !strcmp(argv[1], "-h"))) {
	  printf("usage: %s <inbufSize> <outbufSize>", argv[0]);
	  exit(0);
	}

	/* Start the MATLAB engine */
	if (!(ep = engOpen(NULL))) {
	  printf("Can not start engine\n");
	  exit(-1);
	}

	inputMax = MAXLEN;
	outputMax = 0;

	/* if there are args they might be:
	   1. one string from matlab.el with either 1 or 2 numbers
	   2. one or two strings from a command shell invocation
	*/
	if ( !noArgs ){
	  inputMax = atoi(argv[1]);
	  if (argc>2 && argv[2]) /* DOES happen under matlab.el */
	    outputMax = atoi(argv[2]);
	  else { /*matlab.el probably used to pass args as a single string long time ago */
	    len = strlen(argv[1]);
	    pos = strcspn(argv[1], " \t\n"); /* scan to white space */
	    if (debug) printf("argv[1]=%s len=%d pos=%d\n", argv[1], len, pos);
	    argv[1][pos]=0; /* split */
	    inputMax = atoi(argv[1]);
	    if (pos < len) /* there was stuff left */
	      outputMax = atoi(1+pos+argv[1]);
	  }
	}
	if (!outputMax)		/* nobody set it */
	  outputMax = 8*inputMax;

	inbuf = malloc(inputMax);
	fromEngine = malloc(outputMax);
	engOutputBuffer(ep, fromEngine, outputMax);

	/* Vista+ only :( Any easy way to distinguish pipe from console??? */
	/* if (debug) { */
	/*   if (GetFileInformationByHandleEx(hStdIn, FileNameInfo, inbuf, inputMax)) { */
	/*     wprintf(((PFILE_NAME_INFO)inbuf)->FileName); */
	/*   } */
	/* } */

	printf(">> "); fflush(stdout);
	len = 0;
	while (1) {
	  do {
	    /* accumulate more input if available */
	    if ( !ReadFile(hStdIn, inbuf + len, inputMax - len, &dwRead, NULL)) {
	      printErr();
	      want_quit = 1;
	    } else {
	      len += dwRead;
	      if(len >= inputMax) {
		printf("\nIncrease input buffer size!!!\n");
		want_quit;
	      }
	      if (debug)
		printf("Got %d bytes\n", dwRead);
	    }
	    if (!dwRead || want_quit) {	/* here we "catch" returns */
	      engClose(ep);		/* from signal handler */
	      return 0;	 /* as EIP was likely somewhere in ReadFile. Also return 0 when pipe closes */
	    }
	    printf(">> "); fflush(stdout); /* this makes matlab-mode happy */
	    Sleep(250);
	    if (!PeekNamedPipe(hStdIn, NULL, 0, NULL, &dwRead, NULL)) {
	      printErr();
	      dwRes = WAIT_TIMEOUT;
	    } else {
	      dwRes = dwRead > 0 ? WAIT_OBJECT_0 : WAIT_TIMEOUT;
	    }
	    if (debug) {
	      printf("WAIT_OBJECT_0 == dwRes  = %d\n", WAIT_OBJECT_0 == dwRes);
	    }
	  } while (WAIT_OBJECT_0 == dwRes);

	  if (!len) continue;

	    /* On NT, something erases input and I don't know what. It
	     might be the way comint.el is passing input to this
	     process. If this is platform dependent then other platforms
	     may see doubled input  */

	  //	  printf("%s",inbuf);   fflush(stdout);	/* re-echo input */

	  /* it would be good to test retval, but on NT it seems
	     to return non-zero whether the engine is
	     running or not, contrary to Matlab doc.
	     In fact, I can't figure out how to know whether the
	     engine is running, so special case "exit"
	  */

	  if ('\n' == inbuf[len-1])
	    inbuf[len-1] = 0; /* Matlab on Win32 does not like last empty string */
	  retval = engEvalString(ep, inbuf);
	  if (debug) {
	    printf("retval=%x\n", retval);
	    fflush(stdout);
	  }				/* swap around to make ob-comint happy with eoe */
	  if (retval) {
	    printf("exiting\n"); fflush(stdout);
	    break;
	  }
	  if (fromEngine[0] == 0 ){ /*the command didn't return anything */
	    if(debug)
	      printf("\ncmd returned nothing");
	  }
	  else {
	    char fmt[15];	/* %.99,999,999s>> \x00 symbols max */
	    char *next = fromEngine;
	    const char pattern[] = "\nans =\n\n";
	    /* pcre just for that? alternative is to depend on MS VC++ with #include <regex> */
	    const char pattern2[] = "??? "; /* Undefined function | Subscript indices | Don't use ??? in normal strings :-) */
	    char *pch, *pch2;
	    char want_eoe = 0;
	    do {		/* answer by answer to make ob-octave happy */
	      pch = (char*) strstr(next+1, pattern);
	      pch2 = (char*) strstr(next, pattern2);
	      if (pch2) {
		want_eoe = 1;
		break;
	      }
	      if (pch > next) {
		sprintf(fmt, "%%.%ds>> ", pch - next);
		if (debug)
		  printf("%s next=%0x", fmt, next);
		printf(fmt, next);
		next = pch;
	      }
	    } while (pch != NULL);
	    printf("%s>> ", next); fflush(stdout);
	    if (want_eoe)
	      printf("\nans =\n\norg_babel_eoe\n\n>> "); fflush(stdout);
	    fromEngine[0] = 0; /* clear buffer, else confusing */
	  }
	  len = 0;
        }
        return 0;
}
