#include <libguile.h>

#include "beguile.h"

static bool m_using_guile = false;

beguile::beguile(void)
{
}

beguile::~beguile(void)
{
	if(m_using_guile) scm_gc(); // doesn't collect everything, though
}

void
beguile::init(void)
{
	m_using_guile = true;
	scm_init_guile();
	scm_c_primitive_load("neoleo.scm");
	SCM func = scm_variable_ref(scm_c_lookup("guile-hi"));
	scm_call_0(func);
}

bool
using_guile()
{
	return m_using_guile;
}

void
set_guile_cell_formula(const char* newval, int row, int col)
{
	if(!m_using_guile) return;
        SCM func = scm_variable_ref(scm_c_lookup("set-cell-formula"));
        scm_call_3(func, scm_from_utf8_string(newval), scm_from_int(row), scm_from_int(col));
}



std::string 
get_guile_cell_formula(int row, int col)
{
	if(!m_using_guile) return "Not using guile";
        SCM func = scm_variable_ref(scm_c_lookup("get-cell-formula"));
        SCM ret = scm_call_2(func, scm_from_int(row), scm_from_int(col));
	std::string s = scm_to_utf8_string(ret);

	return s;
}
