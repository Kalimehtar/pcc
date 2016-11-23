(in-package #:pcc)

;;#define NODE P1ND
;;#define tfree p1tfree
;;#define nfree p1nfree
;;#define fwalk p1fwalk

#|
/*
* The following machine-dependent routines may be called during
* initialization:
*
* zbits(OFFSZ, int)    - sets int bits of zero at position OFFSZ.
* infld(CONSZ off, int fsz, CONSZ val)
*                      - sets the bitfield val starting at off and size fsz.
* ninval(CONSZ off, int fsz, NODE *)
*                      - prints an integer constant which may have
*                        a label associated with it, located at off and
*                        size fsz.
*
* Initialization may be of different kind:
* - Initialization at compile-time, all values are constants and laid
*   out in memory. Static or extern variables outside functions.
* - Initialization at run-time, written to their values as code.
*
* Currently run-time-initialized variables are only initialized by using
* move instructions.  An optimization might be to detect that it is
* initialized with constants and therefore copied from readonly memory.
*/

/*
 * The base element(s) of an initialized variable is kept in a linked
 * list, allocated while initialized.
 *
 * When a scalar is found, entries are popped of the instk until it's
 * possible to find an entry for a new scalar; then onstk() is called
 * to get the correct type and size of that scalar.
 *
 * If a right brace is found, pop the stack until a matching left brace
 * were found while filling the elements with zeros.  This left brace is
 * also marking where the current level is for designated initializations.
 *
 * Position entries are increased when traversing back down into the stack.
 */

/*
 * Good-to-know entries from symtab:
 *      soffset - # of bits from beginning of this structure.
 */

/*
 * TO FIX:
 * - Alignment of structs on like i386 char members.
 */

|#

;char *astypnames[] = { 0, 0, "\t.byte", "\t.byte", "\t.short", "\t.short",
;"\t.word", "\t.word", "\t.long", "\t.long", "\t.quad", "\t.quad",
;"ERR", "ERR", "ERR",
					;}

(defun tab (s)
  (concatenate 'string '(#\Tab) s))

(defvar astypnames (make-hash-table))
(mapc (lambda (x)
	(setf (gethash (car x) astypnames) (cadr x)))
      `((UNDEF "")
	(BOOL "")
	(CHAR ,(tab ".byte"))
	(UCHAR ,(tab ".byte"))
	(SHORT ,(tab ".short"))
	(USHORT ,(tab ".short"))
	(INT ,(tab ".word"))
	(UNSIGNED ,(tab ".word"))
	(LONG ,(tab ".long"))
	(ULONG ,(tab ".long"))
	(LONGLONG ,(tab ".quad"))
	(ULONGLONG ,(tab ".quad"))
	(FLOAT ,(tab "ERR"))
	(DOUBLE ,(tab "ERR"))
	(LDOUBLE ,(tab "ERR"))))
