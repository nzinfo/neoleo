#pragma once

// integrating guile into neoleo

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
