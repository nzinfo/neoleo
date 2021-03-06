#include <exception>
#include <getopt.h>
#include <iostream>
#include <string>
#include <unistd.h>
#include <vector>

#include "assert.h"
#include "basic.h"
#include "cmd.h"
#include "config.h"
#include "defuns.h"
#include "experimental.h"
#include "init.h"
#include "io-abstract.h"
#include "io-headless.h"
#include "io-term.h"
#include "io-utils.h"
#include "list.h"
#include "logging.h"
#include "mdi.h"
#include "tests.h"
#include "utils.h"

#include "oleox.hpp"

using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;

#define _(x) (x) // TODO get rid of this line


void
init_native_language_support()
{
#if 0	/* ENABLE_NLS */ // mcarter
	setlocale(LC_ALL, "");
	bindtextdomain(PACKAGE, LOCALEDIR);
	textdomain(PACKAGE);
#endif
}

/* A bland signal handler. */
static RETSIGTYPE
got_sig (int sig)
{
}


static void
init_maps (void)
{
  num_maps = 0;
  the_maps = 0;
  map_names = 0;
  map_prompts = 0;

  the_funcs = (cmd_func**) ck_malloc (sizeof (struct cmd_func *) * 2);
  num_funcs = 1;
  the_funcs[0] = (cmd_func *) get_cmd_funcs();

  find_func (0, &end_macro_cmd, "end-macro");
  find_func (0, &digit_0_cmd, "digit-0");
  find_func (0, &digit_9_cmd, "digit-9");
  find_func (0, &break_cmd, "break");
  find_func (0, &universal_arg_cmd, "universal-argument");

  create_keymap ("universal", 0);
  push_command_frame (0, 0, 0);
}


void
read_init_files()
{
	vector<string> fnames;
	char *home = getenv ("HOME");
	if (home) fnames.push_back(string(home) + "/" + RCFILE);
	fnames.push_back(RCFILE);

	for(const auto& fname:fnames) {
		FILE *fp = fopen(fname.c_str(), "r");
		if(!fp) continue;

		try {
			read_cmds_cmd(fp);
		} catch (OleoJmp& e) {
			cerr << "OleoJmp caught by read_init_file()" << endl;
			string msg = string("   error occured in init file ")
				+ fname
				+ " near line "
				+ std::to_string(Global->sneaky_linec)
				+ "\n";
			const char *m = _(msg.c_str());
			fprintf(stderr, "%s", m);
			io_info_msg(m);
		}

		fclose(fp);
	}
}

void
init_maps_and_macros()
{
	try {
		init_maps();
		init_named_macro_strings ();
                run_init_cmds ();
	} catch (OleoJmp& e) {
		fprintf (stderr, _("Error in the builtin init scripts (a bug!).\n"));
                io_close_display(69);
                exit (69);
	}
}


static bool	option_tests = false;

static bool	option_experimental = false;
std::string	option_tests_argument = "regular";
static char	option_separator = '\t';
static char	*option_format = NULL;
int		option_filter = 0;

bool get_option_tests() { return option_tests;}

static char short_options[] = "4:VqfxHhsFSTvx";
static struct option long_options[] =
{
	{"version",		0,	NULL,	'V'},
	{"experimental",	0,	NULL,	'x'},
	{"quiet",		0,	NULL,	'q'},
	{"ignore-init-file",	0,	NULL,	'f'},
	{"headless",		0,	NULL,	'H'},
	{"help",		0,	NULL,	'h'},
	{"separator",		1,	NULL,	's'},
	{"space",		0,	NULL,	'S'},
	{"format",		1,	NULL,	'F'},
	{"filter",		0,	NULL,	'-'},
	{"tests",		optional_argument,	NULL,	'T'},
	{"version",		0,	NULL,	'v'},
	{NULL,			0,	NULL,	0}
};


void
print_version()
{
	printf(_("%s %s\n"), PACKAGE_NAME, VERSION);
	printf(_("Copyright © 1992-2000 Free Software Foundation, Inc.\n"));
	printf(_("%s comes with ABSOLUTELY NO WARRANTY.\n"), PACKAGE_NAME);
	printf(_("You may redistribute copies of %s\n"), PACKAGE);
	printf(_("under the terms of the GNU General Public License.\n"));
	printf(_("For more information about these matters, "));
	printf(_("see the files named COPYING.\n"));
	printf("\nCompiled: %s %s\n", __DATE__, __TIME__);
	printf("Datadir: %s/neoleo\n", DATADIR);
	// will probably call exit after printing this
}



static void
show_usage (void)
{

  printf(_("This is %s %s\n\n"), PACKAGE, VERSION);

  printf(_("\
Usage: %s [OPTION]... [FILE]...\n\
"), PACKAGE);
  printf(_("\
\n\
  -H, --headless           run without all toolkits\n\
  -h, --help               display this help and exit\n\
  -V, --version            output version information and exit\n\
  -q, --quiet              do not display startup messages\n\
  -f, --ignore-init-file   ignore settings defined in init file\n\
  -s x, --separator x	   set separator for 'list' file type to x\n\
  -S, --space		   set separator for 'list' file type to a space\n\
  -T, --tests [x]          run test suite x\n\
  -F x, --format x	   set default file type to x (oleo, list, sc  ...)\n\
  --filter		   read file from stdin, write to stdout on exit\n\
\n\
Report bugs to <bug-oleo@gnu.org>.\n\
"));
}



void
parse_command_line(int argc, char **argv, volatile int *ignore_init_file)
{
	int opt, optindex;

	while (1) {
		opt = getopt_long (argc, argv, short_options, long_options, &optindex);
		if (opt == EOF)
			break;

#if 0
		if (opt)
			fprintf(stderr, PACKAGE " option %c\n", opt);
		else {
			fprintf(stderr, PACKAGE " optind %d option %s strange ...\n",
					optind, argv[optind]);
		}
#endif

		switch (opt)
		{
			case 'v':
			case 'V':
				print_version();
				exit (0);
				break;
			//case 'q':
			//	spread_quietly = 1;
			//	break;
			case 'f':
				*ignore_init_file = 1;
				break;
			case 'H':
				user_wants_headless = true;
				break;
			case 'h':
				show_usage ();
				exit (0);
				break;
			case 's':
				option_separator = argv[optind][0];
#if 0
				fprintf(stderr, PACKAGE " set list file separator to %c\n", option_separator);
#endif
				optind++;
				list_set_separator(option_separator);
				break;
			case 'S':
				option_separator = ' ';
				list_set_separator(option_separator);
				break;
			case 'F':
#if 0
				fprintf(stderr, "F: optind %d argv[optind] '%s' optopt %d %c\n",
						optind, argv[optind], optopt, optopt);
#endif
				option_format = argv[optind];
				file_set_default_format(option_format);
				optind++;
				break;
			case 'T':
				option_tests = true;
				//cout << "optindex:" << optind << "\n";
				if(!optarg 
						&& optind < argc
						&& NULL !=argv[optind] 
						&& '\0' != argv[optind][0]
						&& '-' != argv[optind][0])
					option_tests_argument = argv[optind++];
				//exit(1);
				break;
			case 'x':
				option_experimental = true;
				break;
			case '-':
				option_filter = 1;
				break;
		}
	}


	if (argc - optind > 1)
	{
		show_usage ();
		exit (1);
	}
}


void run_nonexperimental_mode(int argc, char** argv, int ignore_init_file, int command_line_file)
{
	init_basics();
	headless_graphics(); // fallback position

	if(get_option_tests()) {
		bool all_pass = headless_tests();
		int ret = all_pass ? EXIT_SUCCESS : EXIT_FAILURE;
		//ret = EXIT_FAILURE;
		exit(ret);
	}



	FD_ZERO (&read_fd_set);
	FD_ZERO (&read_pending_fd_set);
	FD_ZERO (&exception_fd_set);
	FD_ZERO (&exception_pending_fd_set);

	bool force_cmd_graphics = false;
	choose_display(force_cmd_graphics);
	io_open_display ();

	OleoSetEncoding(OLEO_DEFAULT_ENCODING);

	init_maps_and_macros();

	oleo_catch_signals(&got_sig);

	execute_command("set-default-format general.float");
	if(!ignore_init_file) read_init_files();

	if (option_filter) {
		read_file_and_run_hooks(stdin, 0, "stdin");
	} else if (argc - optind == 1) {
		if (FILE *fp = fopen (argv[optind], "r")) {
			try {
				read_file_and_run_hooks (fp, 0, argv[optind]);
			} catch (OleoJmp& e) {
				fprintf (stderr, _(", error occured reading '%s'\n"), argv[optind]);
				io_info_msg(_(", error occured reading '%s'\n"), argv[optind]);
			} 
			fclose (fp);
		}

		command_line_file = 1;
		FileSetCurrentFileName(argv[optind]);
		optind++;
	}

	/* Force the command frame to be rebuilt now that the keymaps exist. */
	{
		struct command_frame * last_of_the_old = the_cmd_frame->next;
		while (the_cmd_frame != last_of_the_old)
			free_cmd_frame (the_cmd_frame);
		free_cmd_frame (last_of_the_old);
	}

	io_recenter_cur_win ();
	Global->display_opened = 1;
	io_run_main_loop();
}


int 
main(int argc, char **argv)
{
	int ignore_init_file = 0;
	int command_line_file = 0;	/* was there one? */

	init_native_language_support();
	MdiInitialize();	/* Create initial Global structure */
	InitializeGlobals();
	Global->argc = argc;
	Global->argv = argv;
	parse_command_line(argc, argv, &ignore_init_file);
	if(option_experimental)
		run_experimental_mode();
	else
		run_nonexperimental_mode(argc, argv, ignore_init_file, command_line_file);

	return (0); /* Never Reached! */
}
