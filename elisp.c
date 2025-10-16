#include "elisp.h"
#include <stdio.h>

// Emacs symbols
emacs_value Qt;
emacs_value Qnil;
emacs_value Qnormal;
emacs_value Qbold;
emacs_value Qitalic;
emacs_value Qforeground;
emacs_value Qbackground;
emacs_value Qweight;
emacs_value Qunderline;
emacs_value Qslant;
emacs_value Qreverse;
emacs_value Qstrike;
emacs_value Qextend;
emacs_value Qface;
emacs_value Qbox;
emacs_value Qbar;
emacs_value Qhbar;
emacs_value Qcursor_type;
emacs_value Qemacs_major_version;
emacs_value Qvterm_line_wrap;
emacs_value Qrear_nonsticky;
emacs_value Qvterm_prompt;

// Emacs functions
emacs_value Fblink_cursor_mode;
emacs_value Fsymbol_value;
emacs_value Flength;
emacs_value Flist;
emacs_value Fnth;
emacs_value Ferase_buffer;
emacs_value Finsert;
emacs_value Fgoto_char;
emacs_value Fforward_char;
emacs_value Fforward_line;
emacs_value Fgoto_line;
emacs_value Fdelete_lines;
emacs_value Frecenter;
emacs_value Fset_window_start;
emacs_value Fwindow_start;
emacs_value Fset_window_point;
emacs_value Fwindow_point;
emacs_value Fwindow_body_height;
emacs_value Fpoint;
emacs_value Fminibufferp;
emacs_value Fwindow_buffer;
emacs_value Fapply;

emacs_value Fput_text_property;
emacs_value Fset;
emacs_value Fvterm__flush_output;
emacs_value Fget_buffer_window_list;
emacs_value Fselected_window;
emacs_value Fvterm__set_title;
emacs_value Fvterm__set_directory;
emacs_value Feq;
emacs_value Fvterm__get_color;
emacs_value Fvterm__eval;
emacs_value Fvterm__set_selection;

/* Set the function cell of the symbol named NAME to SFUN using
   the 'fset' function.  */
void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  emacs_value Qfset = env->intern(env, "fset");
  emacs_value Qsym = env->intern(env, name);

  env->funcall(env, Qfset, 2, (emacs_value[]){Qsym, Sfun});
}

/* Provide FEATURE to Emacs.  */
void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");

  env->funcall(env, Qprovide, 1, (emacs_value[]){Qfeat});
}

emacs_value symbol_value(emacs_env *env, emacs_value symbol) {
  return env->funcall(env, Fsymbol_value, 1, (emacs_value[]){symbol});
}

int string_bytes(emacs_env *env, emacs_value string) {
  ptrdiff_t size = 0;
  env->copy_string_contents(env, string, NULL, &size);
  return size;
}

emacs_value length(emacs_env *env, emacs_value string) {
  return env->funcall(env, Flength, 1, (emacs_value[]){string});
}

emacs_value list(emacs_env *env, emacs_value elements[], ptrdiff_t len) {
  return env->funcall(env, Flist, len, elements);
}
emacs_value nth(emacs_env *env, int idx, emacs_value list) {
  emacs_value eidx = env->make_integer(env, idx);
  return env->funcall(env, Fnth, 2, (emacs_value[]){eidx, list});
}

void put_text_property(emacs_env *env, emacs_value string, size_t start, size_t end,
		       emacs_value property, emacs_value value) {
  env->funcall(env, Fput_text_property, 5,
	       (emacs_value[]){env->make_integer(env, start),
			       env->make_integer(env, end),
			       property, value, string});
}

void erase_buffer(emacs_env *env) { env->funcall(env, Ferase_buffer, 0, NULL); }

void insert(emacs_env *env, emacs_value string) {
  env->funcall(env, Finsert, 1, (emacs_value[]){string});
}

void goto_char(emacs_env *env, int pos) {
  emacs_value point = env->make_integer(env, pos);
  env->funcall(env, Fgoto_char, 1, (emacs_value[]){point});
}

void forward_line(emacs_env *env, int n) {
  emacs_value nline = env->make_integer(env, n);
  env->funcall(env, Fforward_line, 1, (emacs_value[]){nline});
}

void goto_line(emacs_env *env, int n) {
  emacs_value nline = env->make_integer(env, n);
  env->funcall(env, Fgoto_line, 1, (emacs_value[]){nline});
}

void delete_lines(emacs_env *env, int linenum, int count, bool del_whole_line) {
  emacs_value Qlinenum = env->make_integer(env, linenum);
  emacs_value Qcount = env->make_integer(env, count);
  env->funcall(env, Fdelete_lines, 3,
	       (emacs_value[]){Qlinenum, Qcount, del_whole_line ? Qt : Qnil});
}

void recenter(emacs_env *env, emacs_value pos) {
  env->funcall(env, Frecenter, 1, (emacs_value[]){pos});
}

emacs_value minibufferp(emacs_env *env, emacs_value buf) {
  return env->funcall(env, Fminibufferp, 2, (emacs_value[]){buf, Qnil});
}

emacs_value window_buffer(emacs_env *env, emacs_value win) {
  return env->funcall(env, Fwindow_buffer, 1, (emacs_value[]){win});
}

emacs_value point(emacs_env *env) {
  return env->funcall(env, Fpoint, 0, NULL);
}

void set_window_start(emacs_env *env, emacs_value win, emacs_value point) {
  env->funcall(env, Fset_window_start, 2, (emacs_value[]){win, point});
}

emacs_value window_start(emacs_env *env, emacs_value win) {
  return env->funcall(env, Fwindow_start, 1, (emacs_value[]){win});
}

void set_window_point(emacs_env *env, emacs_value win, emacs_value point) {
  env->funcall(env, Fset_window_point, 2, (emacs_value[]){win, point});
}

emacs_value window_point(emacs_env *env, emacs_value win) {
  return env->funcall(env, Fwindow_point, 1, (emacs_value[]){win});
}

emacs_value window_body_height(emacs_env *env, emacs_value win) {
  return env->funcall(env, Fwindow_body_height, 1, (emacs_value[]){win});
}

bool eq(emacs_env *env, emacs_value e1, emacs_value e2) {
  emacs_value Qeq = env->funcall(env, Feq, 2, (emacs_value[]){e1, e2});
  return env->is_not_nil(env, Qeq);
}

void forward_char(emacs_env *env, emacs_value n) {
  env->funcall(env, Fforward_char, 1, (emacs_value[]){n});
}

emacs_value get_buffer_window_list(emacs_env *env) {
  return env->funcall(env, Fget_buffer_window_list, 3,
                      (emacs_value[]){Qnil, Qnil, Qt});
}

emacs_value selected_window(emacs_env *env) {
  return env->funcall(env, Fselected_window, 0, (emacs_value[]){});
}

void set_cursor_type(emacs_env *env, emacs_value cursor_type) {
  env->funcall(env, Fset, 2, (emacs_value[]){Qcursor_type, cursor_type});
}

void set_cursor_blink(emacs_env *env, bool blink) {
  env->funcall(env, Fblink_cursor_mode, 1,
               (emacs_value[]){env->make_integer(env, blink)});
}

emacs_value vterm_get_color(emacs_env *env, int index, emacs_value args) {
  emacs_value idx = env->make_integer(env, index);
  return env->funcall(env, Fapply, 3, (emacs_value[]){ Fvterm__get_color, idx, args });
}

void set_title(emacs_env *env, emacs_value string) {
  env->funcall(env, Fvterm__set_title, 1, (emacs_value[]){string});
}

void set_directory(emacs_env *env, emacs_value string) {
  env->funcall(env, Fvterm__set_directory, 1, (emacs_value[]){string});
}

emacs_value vterm_eval(emacs_env *env, emacs_value string) {
  return env->funcall(env, Fvterm__eval, 1, (emacs_value[]){string});
}

emacs_value vterm_set_selection(emacs_env *env, emacs_value selection_target,
                                emacs_value selection_data) {
  return env->funcall(env, Fvterm__set_selection, 2,
                      (emacs_value[]){selection_target, selection_data});
}
