#pragma once

// integrating guile into neoleo

#include <string>

class beguile {
	public:
		beguile();
		~beguile();
		void init();
		bool using_guile();
	private:
};

bool using_guile();
void set_guile_cell_formula(const char* newval, int row, int col);
std::string get_guile_cell_formula(int row, int col);
