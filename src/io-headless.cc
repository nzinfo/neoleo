//#define __GNU_SOURCE // we want TEMP_FAILURE_RETRY defined
//#include <array>
#include <assert.h>
#include <algorithm>
#include <errno.h>
#include <functional>
#include <map>
#include <ncurses.h>
#include <unistd.h>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>


#include "basic.h"
#include "beguile.h"
#include "cell.h"
#include "io-abstract.h"
#include "io-headless.h"
#include "io-visual.h"
#include "cmd.h"
#include "window.h"
#include "oleox.hpp"
#include "io-curses.h"
#include "io-utils.h"
#include "lists.h"
#include "tbl.h"
#include "utils.h"
#include "shell.h"


using std::cin;
using std::cout;
using std::endl;
using std::function;
using std::map;
using std::vector;

//using T = int;
typedef int T;

string to_hex(long n)
{
	std::stringstream ss;
	ss << std::hex << n << "h";
	return ss.str();
}

string to_oct(long n)
{
	std::stringstream ss;
	ss << "0o" << std::oct << n;
	return ss.str();
}

static void 
do_nothing(void)
{
	return;
}


static void
_io_open_display(void)
{
	/* We fake having a window. This is important because io_init_windows()
	 * will do things like set nwin = 1
	 *
	 * The init was cribbed from io-curses.c
	 */

	int assumed_lines = 24, assumed_cols = 80;
	io_init_windows(assumed_lines, assumed_cols, 1, 2, 1, 1, 1, 1);

}

static int m_nrow = 1;
static void
XXX_set_curow(int nrow)
{
	m_nrow = nrow;
}

static int m_ncol = 1;
static void
XXX_set_cucol(int ncol)
{
	m_ncol = ncol;
}

static void
_io_update_status(void)
{
	//puts("Called _io_update_status()");
}

static void
_io_repaint_win (struct window *win)
{
  //io_repaint ();
}



static void
_io_fix_input(void)
{
	//puts("Entering _io_fix_input()");
}


// suggested at
// http://stackoverflow.com/questions/8302547/temp-failure-retry-and-use-gnu
#define CALL_RETRY(retvar, expression) do { \
    retvar = (expression); \
} while (retvar == -1 && errno == EINTR);


static int
_io_input_avail(void)
{
	int filedes = STDIN_FILENO;
	unsigned int seconds = 0;

	// taken from https://www.gnu.org/software/libc/manual/html_node/Waiting-for-I_002fO.html
	

	fd_set set;
	struct timeval timeout;


	/* Initialize the file descriptor set. */
	FD_ZERO (&set);
	FD_SET (filedes, &set);

	/* Initialize the timeout data structure. */
	timeout.tv_sec = seconds;
	timeout.tv_usec = 0;

	/* select returns 0 if timeout, 1 if input available, -1 if error. 
	 * Use CALL_RETRY rather than TEMP_FAILURE_RETRY (see above) */
	int res;
	CALL_RETRY (res, select (FD_SETSIZE,
				&set, NULL, NULL,
				&timeout));
	return res;
}


// TODO: this was a copy-pasta from io_curses.c
static void 
_curses_io_scan_for_input (int block)
{
  /* This function only exists because X kbd events don't generate
   * SIGIO. Under curses, the SIGIO hander does the work of this
   * function.
   * Attempt to have the curses mode be somewhat responsive even in
   * the presence of an endless loop by explicitly looking for events
   * here.
   */
  struct timeval tv;

  tv.tv_sec = 0;
  tv.tv_usec = 1000;
  block_until_excitement(&tv);
}


static int
_io_read_kbd(char *buf, int size)
{        
  //int r = read (0, buf, size);
  int r = read (STDIN_FILENO, buf, size);
  //FD_CLR (0, &read_pending_fd_set);
  //FD_CLR (0, &exception_pending_fd_set);
  return r;
}      

static void
_io_insert (int len)
{ 
  //iv_insert (&input_view, len);
} 

static void
_io_pr_cell_win (struct window *win, CELLREF r, CELLREF c, CELL *cp)
{
}

static void 
info(int fildes)
{
	// print diagnostic information
	
	typedef struct info_t { string str; int num; string desc; } info_t;
	auto infos = vector<info_t> {
		{"KEY_END",	KEY_END,	"End key"},
		{"KEY_HOME",	KEY_HOME,	"Home key"},
		{"KEY_LEFT", 	KEY_LEFT, 	"Arrow left"},
		{"KEY_NPAGE",	KEY_NPAGE}
	};

	for(const auto& i:infos)
		cout << "curses." << pad_right(i.str,11)  << pad_left(to_oct(i.num),6) 
			<< pad_left(to_hex(i.num), 7)  
			<< pad_left(std::to_string(int(i.num)), 5) 
			<< "     # " << i.desc << "\n";
}

static void
insert_columnwise(T fildes)
{
	//FILE *fp=0; // std::cin
	//for(std::string line; line = getline_from_fildes(fildes);){
	std::string line;
	while(true) {
		bool eof;
		line = getline_from_fildes(fildes, eof);
		if(line == ".") break;
		if(line == ";") {
			curow = 1;
			cucol++;
			continue;
		}
		if(line.size() ==0) {
			curow++;
			continue;
		}
		if(line[0] == '#') continue;

		edit_cell_at(curow, cucol, line);
		curow++;
		
		//cout << "You said " << line <<  (line != "." ) << endl;
		if(eof) return;
	}
}
static void
insert_rowwise(T fildes)
{
	std::string line;
	while(true) {
		bool eof;
		line = getline_from_fildes(fildes, eof);
		if(line == ".") break;
		if(line == ";") {
			curow++;
			cucol=1;
			continue;
		}
		if(line.size() ==0) {
			cucol++;
			continue;
		}
		if(line[0] == '#') continue;

		edit_cell_at(curow, cucol, line);
		cucol++;
		
		//cout << "You said " << line <<  (line != "." ) << endl;
		if(eof) return;
	}
}



static void type_cell(int fildes)
{
	string s = using_guile()? get_guile_cell_formula(curow, cucol) : 
		print_cell(find_cell(curow, cucol));

	cout	<<  s	<< "\n";
}


static void write_file(int fildes)
{
	string name = FileGetCurrentFileName();
	FILE *fp = fopen(name.c_str(), "w");
	assert(fp);
	write_cmd(fp, name.c_str());
	fclose(fp);

}

// this seems to crash
function<void(int)> with_int(function<void()> fn)
{
	//return std::bind(fn, 0);
	//return fn;
	return [&](int i) { fn(); };
}

static void hless_tbl(int fildes)
{
	tbl();
}

static void type_dsv(int fildes)
{
	save_dsv(stdout, 0);
}

static map<string, function<void(T)> > func_map = {
	{"colours", colours},
	{"I", insert_rowwise},
	{"i", insert_columnwise},
	{"info", info},
	{"kt", keyboard_test},
	{"tbl", hless_tbl},
	//{"tbl", with_int(tbl)},
	{"type-cell", type_cell},
	{"type-dsv", type_dsv},
	{"view", show_cells},
	{"vi", visual_mode},
	{"w", write_file}
};

bool
process_headless_line(std::string line, int fildes)
{
	//cout << "process_headless_line: " << line << endl;

	// try to find a canned function and execute it
	auto it = func_map.find(line);
	if(it != func_map.end()) {
		auto fn = it->second;
		fn(fildes);
		//(it->second)();
		cout << std::flush;
		return true;
	}


	/*if(line == "bye") {
	  cout << "300 OK bye" << endl;
	  return;
	  }
	  */
	if(line == "q") {
	       	//cout << "300 OK bye" << endl;
		return false;
	}

	try {
		//cout << "process_headless_line: execute_command:" << line << endl;
		execute_command((char*) line.c_str());
		//std::cout << "100 OK" << endl;
	} catch (const OleoJmp&) {
		//cout << "200 FAIL Caught OleoJmp" << endl;
		cout << "?\n";
	}

	cout << std::flush;
	return true;
}

static void
_io_run_main_loop()
{
	//cout << "100 OK Type 'bye' to exit" << endl;


	//cout << "100 OK" << "\n";
	std::string line;
	constexpr int fildes = STDIN_FILENO;
	//while(getline(std::cin, line)) {
	bool cont = true;
	while(cont) {
		bool eof;
		//cout << "_io_run_main_loop:about to call getline_from_fildes" << endl;
		line = getline_from_fildes(fildes, eof);
		//cout << "_io_run_main_loop:line:" << line << endl;
		//if(line == "q")
		//	return;
		//else
		cont =	process_headless_line(line, fildes);
		//cout << "_io_run_main_loop: cont:" << cont << endl;
		if(eof) {
		       	//cout << "_io_run_main_loop: eof" << endl;
			cont = false;
		}
	}
	
}

static void
_io_bell()
{
	//cout << "BELL" << endl;
}

void
headless_graphics(void)
{
	// not sure if the following are useful:
	//FD_SET (0, &read_fd_set);
	//FD_SET (0, &exception_fd_set);
	
	/* I'm bored by most of this, although it is probably (?) useful
	 */

	//io_command_loop = _io_headless_command_loop;

	io_open_display = _io_open_display;
	//io_open_display = do_nothing;

	//io_redisp = _io_redisp;
	io_redisp = do_nothing;

	io_repaint = do_nothing;

	io_repaint_win = _io_repaint_win;
	//io_close_display = _io_close_display;
	io_input_avail = _io_input_avail;
	io_scan_for_input = _curses_io_scan_for_input;
	//io_wait_for_input = _io_wait_for_input;
	io_read_kbd = _io_read_kbd;
	//io_nodelay = _io_nodelay;
	//io_getch = _io_getch;
	io_bell = _io_bell;
	//io_get_chr = _io_get_chr;

	io_update_status = _io_update_status;
	io_fix_input = _io_fix_input;
	//io_move_cursor = _io_move_cursor;
	//io_erase = _io_erase;
	io_insert = _io_insert;
	//io_over = _io_over;
	io_flush = do_nothing;
	//io_clear_input_before = _io_clear_input_before;
	//io_clear_input_after = _io_clear_input_after;
	io_pr_cell_win = _io_pr_cell_win;
	io_hide_cell_cursor = do_nothing;
	//io_cellize_cursor = _io_cellize_cursor;
	//io_inputize_cursor = _io_inputize_cursor;
	io_display_cell_cursor = do_nothing;


	
	//set_headless(true);
	//io_recenter_cur_win = do_nothing;
	//io_recenter_all_win = do_nothing;
	//set_curow = _set_curow;
	//set_cucol = _set_cucol;

	//io_run_main_loop = fairly_std_main_loop;
	io_run_main_loop = _io_run_main_loop;
	//nwin = 1;
}

