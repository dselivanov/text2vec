#ifndef SPARSE_MATRIX_H
#define SPARSE_MATRIX_H

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <algorithm>
#include <vector>
#include <cmath>
#include <assert.h>
#include <Rcpp.h>
// #include <omp.h>

//#include "zlib_util.h"

#define MALLOC(type, size) (type*)malloc(sizeof(type)*(size))

#define smat_t sparse_matrix
template<typename val_type> class smat_t;
template<typename val_type> class entry_iterator_t; // iterator for files with (i,j,v) tuples
template<typename val_type> class smat_iterator_t; // iterator for nonzero entries in smat_t
template<typename val_type> class smat_subset_iterator_t; // iterator for nonzero entries in a subset

// H = X*W, (X: m*n, W: n*k row-major, H m*k row major)
template<typename val_type> void smat_x_dmat(const smat_t<val_type> &X, const val_type* W, const size_t k, val_type *H);
// H = a*X*W + H0, (X: m*n, W: n*k row-major, H m*k row major)
template<typename val_type> void smat_x_dmat(const val_type a, const smat_t<val_type> &X, const val_type* W, const size_t k, const val_type *H0, val_type *H);

// Sparse matrix format CSC & CSR
template<typename val_type>
class smat_t{
	private:
		bool mem_alloc_by_me;
		bool read_from_binary;
		unsigned char* binary_buf;
		size_t binary_buf_len;
		const static int HeaderSize =
			sizeof(size_t)+sizeof(size_t)+sizeof(size_t)+sizeof(size_t);
		void csr_to_csc();
		void csc_to_csr();
	public:
		size_t rows, cols;
		size_t nnz, max_row_nnz, max_col_nnz;
		val_type *val, *val_t;
		size_t *col_ptr, *row_ptr;
		unsigned *row_idx, *col_idx;

		// filetypes for loading smat_t
		enum format_t {TXT=0, PETSc=1, BINARY=2, COMPRESSION=3, R=4};

		// Constructor and Destructor
		smat_t() : mem_alloc_by_me(false), read_from_binary(false), rows(0), cols(0), nnz(0){
		val=val_t=NULL; col_ptr=row_ptr=NULL, row_idx=col_idx=NULL;}
		smat_t(const smat_t& m){*this = m; mem_alloc_by_me = false; read_from_binary = false;}
		~smat_t(){ clear_space();}

		void clear_space();
		smat_t transpose();
		void apply_permutation(const std::vector<unsigned> &row_perm, const std::vector<unsigned> &col_perm);
		void apply_permutation(const unsigned *row_perm=NULL, const unsigned *col_perm=NULL);
		smat_subset_iterator_t<val_type> row_subset_it(const std::vector<unsigned> &subset);
		smat_subset_iterator_t<val_type> row_subset_it(const unsigned *subset, int subset_size);
		smat_subset_iterator_t<val_type> col_subset_it(const std::vector<unsigned> &subset);
		smat_subset_iterator_t<val_type> col_subset_it(const unsigned *subset, int subset_size);
		smat_t row_subset(const std::vector<unsigned> &subset);
		smat_t row_subset(const unsigned *subset, int subset_size);


		size_t nnz_of_row(unsigned i) const {return (row_ptr[i+1]-row_ptr[i]);}
		size_t nnz_of_col(unsigned i) const {return (col_ptr[i+1]-col_ptr[i]);}

		// smat-vector multiplication
		void Xv(const val_type *v, val_type *Xv);
		void XTu(const val_type *u, val_type *XTu);

		// IO methods
		void load_from_iterator(size_t _rows, size_t _cols, size_t _nnz, entry_iterator_t<val_type>* entry_it);
		void load(size_t _rows, size_t _cols, size_t _nnz, const char *filename, format_t fmt);
		void load_from_PETSc(const char  *filename);
		void load_from_R(Rcpp::S4 dtm);
		void save_PETSc_to_file(const char *filename);
		void load_from_binary(const char *filename);
		void save_binary_to_file(const char *filename);

		// used for MPI verions
		void from_mpi(){
			mem_alloc_by_me = true;
			max_col_nnz = 0;
			for(size_t c = 0; c < cols; c++)
				max_col_nnz = std::max(max_col_nnz, nnz_of_col(c));
		}
		val_type get_global_mean() const;
		void remove_bias(val_type bias=0);
};


/*-------------- Iterators -------------------*/

template<typename val_type>
class rate_t{
	public:
		unsigned i, j; val_type v, weight;
		rate_t(int ii=0, int jj=0, val_type vv=0, val_type ww=1.0): i(ii), j(jj), v(vv), weight(ww){}
};

template<typename val_type>
class entry_iterator_t {
	public:
		size_t nnz;
		virtual rate_t<val_type> next() = 0;
};

#define MAXLINE 10240
// Iterator for files with (i,j,v) tuples
template<typename val_type>
class file_iterator_t: public entry_iterator_t<val_type>{
	public:
		file_iterator_t(size_t nnz_, const char* filename, size_t start_pos=0);
		~file_iterator_t(){ if (fp) fclose(fp); }
		rate_t<val_type> next();
	private:
		size_t nnz;
		FILE *fp;
		char line[MAXLINE];
};

// smat_t iterator
template<typename val_type>
class smat_iterator_t: public entry_iterator_t<val_type>{
	public:
		enum {ROWMAJOR, COLMAJOR};
		// major: smat_iterator_t<val_type>::ROWMAJOR or smat_iterator_t<val_type>::COLMAJOR
		smat_iterator_t(const smat_t<val_type>& M, int major = ROWMAJOR);
		~smat_iterator_t() {}
		rate_t<val_type> next();
	private:
		size_t nnz;
		unsigned *col_idx;
		size_t *row_ptr;
		val_type *val_t;
		size_t rows, cols, cur_idx;
		size_t cur_row;
};

// smat_t subset iterator
template<typename val_type>
class smat_subset_iterator_t: public entry_iterator_t<val_type>{
	public:
		enum {ROWMAJOR, COLMAJOR};
		// major: smat_iterator_t<val_type>::ROWMAJOR or smat_iterator_t<val_type>::COLMAJOR
		smat_subset_iterator_t(const smat_t<val_type>& M, const unsigned *subset, size_t size, bool remapping=false, int major = ROWMAJOR);
		~smat_subset_iterator_t() {}
		size_t get_nnz() {return nnz;}
		size_t get_rows() {return major==ROWMAJOR? remapping? subset.size(): rows: rows;}
		size_t get_cols() {return major==ROWMAJOR? cols: remapping? subset.size():cols;}
		rate_t<val_type> next();
	private:
		size_t nnz;
		unsigned *col_idx;
		size_t *row_ptr;
		val_type *val_t;
		size_t rows, cols, cur_idx;
		size_t cur_row;
		std::vector<unsigned>subset;
		int major;
		bool remapping;
};

// -------------- Implementation --------------
template<typename val_type>
void smat_t<val_type>::clear_space() {
	if(mem_alloc_by_me) {
		if(read_from_binary)
			free(binary_buf);
		else {
			if(val)free(val); if(val_t)free(val_t);
			if(row_ptr)free(row_ptr);if(row_idx)free(row_idx);
			if(col_ptr)free(col_ptr);if(col_idx)free(col_idx);
		}
	}
	read_from_binary = false;
	mem_alloc_by_me = false;
}

template<typename val_type>
smat_t<val_type> smat_t<val_type>::transpose(){
	smat_t<val_type> mt;
	mt.cols = rows; mt.rows = cols; mt.nnz = nnz;
	mt.val = val_t; mt.val_t = val;
	mt.col_ptr = row_ptr; mt.row_ptr = col_ptr;
	mt.col_idx = row_idx; mt.row_idx = col_idx;
	mt.max_col_nnz=max_row_nnz; mt.max_row_nnz=max_col_nnz;
	return mt;
}

template<typename val_type>
void smat_t<val_type>::apply_permutation(const std::vector<unsigned> &row_perm, const std::vector<unsigned> &col_perm) {
	apply_permutation(row_perm.size()==rows? &row_perm[0]: NULL, col_perm.size()==cols? &col_perm[0]: NULL);
}

template<typename val_type>
void smat_t<val_type>::apply_permutation(const unsigned *row_perm, const unsigned *col_perm) {
	if(row_perm!=NULL) {
		for(size_t idx = 0; idx < nnz; idx++) row_idx[idx] = row_perm[row_idx[idx]];
		csc_to_csr();
		csr_to_csc();
	}
	if(col_perm!=NULL) {
		for(size_t idx = 0; idx < nnz; idx++) col_idx[idx] = col_perm[col_idx[idx]];
		csr_to_csc();
		csc_to_csr();
	}
}

template<typename val_type>
smat_subset_iterator_t<val_type> smat_t<val_type>::row_subset_it(const std::vector<unsigned> &subset) {
	return row_subset_it(&subset[0], (int)subset.size());
}

template<typename val_type>
smat_subset_iterator_t<val_type> smat_t<val_type>::row_subset_it(const unsigned *subset, int subset_size) {
	return smat_subset_iterator_t<val_type> (*this, subset, subset_size);
}

template<typename val_type>
smat_subset_iterator_t<val_type> smat_t<val_type>::col_subset_it(const std::vector<unsigned> &subset) {
	return col_subset_it(&subset[0], (int)subset.size());
}

template<typename val_type>
smat_subset_iterator_t<val_type> smat_t<val_type>::col_subset_it(const unsigned *subset, int subset_size) {
	bool remmapping = false; // no remapping by default
	return smat_subset_iterator_t<val_type> (*this, subset, subset_size, remmapping, smat_subset_iterator_t<val_type>::COLMAJOR);
}

template<typename val_type>
smat_t<val_type> smat_t<val_type>::row_subset(const std::vector<unsigned> &subset) {
	return row_subset(&subset[0], (int)subset.size());
}

template<typename val_type>
smat_t<val_type> smat_t<val_type>::row_subset(const unsigned *subset, int subset_size) {
	smat_subset_iterator_t<val_type> it(*this, subset, subset_size);
	smat_t<val_type> sub_smat;
	sub_smat.load_from_iterator(subset_size, cols, it.get_nnz(), &it);
	return sub_smat;
}

template<typename val_type>
val_type smat_t<val_type>::get_global_mean() const {
	val_type sum=0;
	for(size_t idx = 0; idx < nnz; idx++) sum += val[idx];
	return sum/(val_type)nnz;
}

template<typename val_type>
void smat_t<val_type>::remove_bias(val_type bias){
	if(bias) {
		for(size_t idx = 0; idx < nnz; idx++) {
			val[idx] -= bias;
			val_t[idx] -= bias;
		}
	}
}

template<typename val_type>
void smat_t<val_type>::Xv(const val_type *v, val_type *Xv) {
	for(size_t i = 0; i < rows; ++i) {
		Xv[i] = 0;
		for(size_t idx = row_ptr[i]; idx < row_ptr[i+1]; ++idx)
			Xv[i] += val_t[idx] * v[col_idx[idx]];
	}
}

template<typename val_type>
void smat_t<val_type>::XTu(const val_type *u, val_type *XTu) {
	for(size_t i = 0; i < cols; ++i) {
		XTu[i] = 0;
		for(size_t idx = col_ptr[i]; idx < col_ptr[i+1]; ++idx)
			XTu[i] += val[idx] * u[row_idx[idx]];
	}
}

// Comparator for sorting rates into row/column comopression storage
template<typename val_type>
class SparseComp {
	public:
		const unsigned *row_idx;
		const unsigned *col_idx;
		SparseComp(const unsigned *row_idx_, const unsigned *col_idx_, bool isCSR=true) {
			row_idx = (isCSR)? row_idx_: col_idx_;
			col_idx = (isCSR)? col_idx_: row_idx_;
		}
		bool operator()(size_t x, size_t y) const {
			return  (row_idx[x] < row_idx[y]) || ((row_idx[x] == row_idx[y]) && (col_idx[x]< col_idx[y]));
		}
};

template<typename val_type>
void smat_t<val_type>::load_from_iterator(size_t _rows, size_t _cols, size_t _nnz, entry_iterator_t<val_type> *entry_it){
	clear_space(); // clear any pre-allocated space in case of memory leak
	rows =_rows,cols=_cols,nnz=_nnz;
	mem_alloc_by_me = true;
	val = MALLOC(val_type, nnz); val_t = MALLOC(val_type, nnz);
	row_idx = MALLOC(unsigned, nnz); col_idx = MALLOC(unsigned, nnz);
	//row_idx = MALLOC(unsigned long, nnz); col_idx = MALLOC(unsigned long, nnz); // switch to this for matlab
	row_ptr = MALLOC(size_t, rows+1); col_ptr = MALLOC(size_t, cols+1);
	memset(row_ptr,0,sizeof(size_t)*(rows+1));
	memset(col_ptr,0,sizeof(size_t)*(cols+1));

	// a trick here to utilize the space the have been allocated
	std::vector<size_t> perm(_nnz);
	unsigned *tmp_row_idx = col_idx;
	unsigned *tmp_col_idx = row_idx;
	val_type *tmp_val = val;
	for(size_t idx = 0; idx < _nnz; idx++){
		rate_t<val_type> rate = entry_it->next();
		row_ptr[rate.i+1]++;
		col_ptr[rate.j+1]++;
		tmp_row_idx[idx] = rate.i;
		tmp_col_idx[idx] = rate.j;
		tmp_val[idx] = rate.v;
		perm[idx] = idx;
	}
	// sort entries into row-majored ordering
	sort(perm.begin(), perm.end(), SparseComp<val_type>(tmp_row_idx, tmp_col_idx, true));
	// Generate CSR format
	for(size_t idx = 0; idx < _nnz; idx++) {
		val_t[idx] = tmp_val[perm[idx]];
		col_idx[idx] = tmp_col_idx[perm[idx]];
	}

	// Calculate nnz for each row and col
	max_row_nnz = max_col_nnz = 0;
	for(size_t r = 1; r <= rows; r++) {
		max_row_nnz = std::max(max_row_nnz, row_ptr[r]);
		row_ptr[r] += row_ptr[r-1];
	}
	for(size_t c = 1; c <= cols; c++) {
		max_col_nnz = std::max(max_col_nnz, col_ptr[c]);
		col_ptr[c] += col_ptr[c-1];
	}

	// Transpose CSR into CSC matrix
	for(size_t r = 0; r < rows; ++r){
		for(size_t idx = row_ptr[r]; idx < row_ptr[r+1]; idx++){
			size_t c = (size_t) col_idx[idx];
			row_idx[col_ptr[c]] = r;
			val[col_ptr[c]++] = val_t[idx];
		}
	}
	for(size_t c = cols; c > 0; --c) col_ptr[c] = col_ptr[c-1];
	col_ptr[0] = 0;
}
template<typename val_type>
void smat_t<val_type>::load_from_R(Rcpp::S4 dtm) {
  clear_space(); // clear any pre-allocated space in case of memory leak
  rows = dtm.slot("Dim");
  //cols= _cols;
  //nnz= _nnz;
  mem_alloc_by_me = true;
  val = MALLOC(val_type, nnz);
  val_t = MALLOC(val_type, nnz);
  row_idx = MALLOC(unsigned, nnz);
  col_idx = MALLOC(unsigned, nnz);
  row_ptr = MALLOC(size_t, rows+1);
  col_ptr = MALLOC(size_t, cols+1);
  memset(row_ptr, 0, sizeof(size_t) * (rows + 1));
  memset(col_ptr, 0, sizeof(size_t) * (cols + 1));
}



template<typename val_type>
void smat_t<val_type>::load(size_t _rows, size_t _cols, size_t _nnz, const char* filename, smat_t<val_type>::format_t fmt){

	if(fmt == smat_t<val_type>::TXT) {
		file_iterator_t<val_type> entry_it(_nnz, filename);
		load_from_iterator(_rows, _cols, _nnz, &entry_it);
	} else if(fmt == smat_t<val_type>::PETSc) {
		load_from_PETSc(filename);
	} else if(fmt == smat_t<val_type>::R) {
	  void load_from_R(Rcpp::S4 dtm);
	}
	  else {
		fprintf(stderr, "Error: filetype %d not supported\n", fmt);
		return ;
	}
}

template<typename val_type>
void smat_t<val_type>::save_PETSc_to_file(const char *filename){
	const int UNSIGNED_FILE = 1211216, LONG_FILE = 1015;
	FILE *fp = fopen(filename, "wb");
	if(fp == NULL) {
		fprintf(stderr,"Error: can't open file %s\n", filename);
		exit(1);
	}
	int32_t int_buf[3] = {(int32_t)LONG_FILE, (int32_t)rows, (int32_t)cols};
	std::vector<int32_t> nnz_row(rows);
	for(size_t r = 0; r < rows; r++)
		nnz_row[r] = (int)nnz_of_row(r);

	fwrite(&int_buf[0], sizeof(int32_t), 3, fp);
	fwrite(&nnz, sizeof(size_t), 1, fp);
	fwrite(&nnz_row[0], sizeof(int32_t), rows, fp);
	fwrite(&col_idx[0], sizeof(unsigned), nnz, fp);

	// the following part == fwrite(val_t, sizeof(double), nnz, fp);
	const size_t chunksize = 1024;
	double buf[chunksize];
	size_t idx = 0;
	while(idx + chunksize < nnz) {
		for(size_t i = 0; i < chunksize; i++)
			buf[i] = (double) val_t[idx+i];
		fwrite(&buf[0], sizeof(double), chunksize, fp);
		idx += chunksize;
	}
	size_t remaining = nnz - idx;
	for(size_t i = 0; i < remaining; i++)
		buf[i] = (double) val_t[idx+i];
	fwrite(&buf[0], sizeof(double), remaining, fp);

	fclose(fp);
}

template<typename val_type>
void smat_t<val_type>::load_from_PETSc(const char *filename) {
	clear_space(); // clear any pre-allocated space in case of memory leak
	const int UNSIGNED_FILE = 1211216, LONG_FILE = 1015;
	int32_t int_buf[3];
	size_t headersize = 0;
	FILE *fp = fopen(filename, "rb");
	if(fp == NULL) {
		fprintf(stderr, "Error: can't read the file (%s)!!\n", filename);
		return;
	}
	headersize += sizeof(int)*fread(int_buf, sizeof(int), 3, fp);
	int filetype = int_buf[0];
	rows = (size_t) int_buf[1];
	cols = (size_t) int_buf[2];
	if(filetype == UNSIGNED_FILE) {
		headersize += sizeof(int)*fread(int_buf, sizeof(int32_t), 1, fp);
		nnz = (size_t) int_buf[0];
	} else if (filetype == LONG_FILE){
		headersize += sizeof(size_t)*fread(&nnz, sizeof(int64_t), 1, fp);
	} else {
		fprintf(stderr, "Error: wrong PETSc format for %s\n", filename);
	}
	// Allocation of memory
	mem_alloc_by_me = true;
	val = MALLOC(val_type, nnz); val_t = MALLOC(val_type, nnz);
	row_idx = MALLOC(unsigned, nnz); col_idx = MALLOC(unsigned, nnz);
	row_ptr = MALLOC(size_t, rows+1); col_ptr = MALLOC(size_t, cols+1);

	// load CSR from the binary PETSc format
	{
		// read row_ptr
		std::vector<int32_t> nnz_row(rows);
		headersize += sizeof(int32_t)*fread(&nnz_row[0], sizeof(int32_t), rows, fp);
		row_ptr[0] = 0;
		for(size_t r = 1; r <= rows; r++)
			row_ptr[r] = row_ptr[r-1] + nnz_row[r-1];
		// read col_idx
		headersize += sizeof(int)*fread(&col_idx[0], sizeof(unsigned), nnz, fp);

		// read val_t
		const size_t chunksize = 1024;
		double buf[chunksize];
		size_t idx = 0;
		while(idx + chunksize < nnz) {
			headersize += sizeof(double)*fread(&buf[0], sizeof(double), chunksize, fp);
			for(size_t i = 0; i < chunksize; i++)
				val_t[idx+i] = (val_type) buf[i];
			idx += chunksize;
		}
		size_t remaining = nnz - idx;
		headersize += sizeof(double)*fread(&buf[0], sizeof(double), remaining, fp);
		for(size_t i = 0; i < remaining; i++)
			val_t[idx+i] = (val_type) buf[i];
	}
	fclose(fp);

	csr_to_csc(); // Convert CSR to CSC
	max_row_nnz = max_col_nnz = 0;
	for(size_t c = 0; c < cols; c++) max_col_nnz = std::max(max_col_nnz, nnz_of_col(c));
	for(size_t r = 0; r < rows; r++) max_row_nnz = std::max(max_row_nnz, nnz_of_row(r));
}

template<typename val_type>
void smat_t<val_type>::csr_to_csc() {
	memset(col_ptr, 0, sizeof(size_t)*(cols+1));
	for(size_t idx = 0; idx < nnz; idx++)
		col_ptr[col_idx[idx]+1]++;
	for(size_t c = 1; c <= cols; c++)
		col_ptr[c] += col_ptr[c-1];
	for(size_t r = 0; r < rows; r++) {
		for(size_t idx = row_ptr[r]; idx != row_ptr[r+1]; idx++) {
			size_t c = (size_t) col_idx[idx];
			row_idx[col_ptr[c]] = r;
			val[col_ptr[c]++] = val_t[idx];
		}
	}
	for(size_t c = cols; c > 0; c--)
		col_ptr[c] = col_ptr[c-1];
	col_ptr[0] = 0;
}

template<typename val_type>
void smat_t<val_type>::csc_to_csr() {
	memset(row_ptr, 0, sizeof(size_t)*(rows+1));
	for(size_t idx = 0; idx < nnz; idx++)
		row_ptr[row_idx[idx]+1]++;
	for(size_t r = 1; r <= rows; r++)
		row_ptr[r] += row_ptr[r-1];
	for(size_t c = 0; c < cols; c++) {
		for(size_t idx = col_ptr[c]; idx != col_ptr[c+1]; idx++) {
			size_t r = (size_t) row_idx[idx];
			col_idx[row_ptr[r]] = c;
			val_t[row_ptr[r]++] = val[idx];
		}
	}
	for(size_t r = rows; r > 0; r--)
		row_ptr[r] = row_ptr[r-1];
	row_ptr[0] = 0;
}

template<typename val_type>
file_iterator_t<val_type>::file_iterator_t(size_t nnz_, const char* filename, size_t start_pos){
	nnz = nnz_;
	fp = fopen(filename,"rb");
	if(fp == NULL) {
		fprintf(stderr, "Error: cannot read the file (%s)!!\n", filename);
		return;
	}
	fseek(fp, start_pos, SEEK_SET);
}

template<typename val_type>
rate_t<val_type> file_iterator_t<val_type>::next() {
	const int base10 = 10;
	if(nnz > 0) {
		--nnz;
		if(fgets(&line[0], MAXLINE, fp)==NULL)
			fprintf(stderr, "Error: reading error !!\n");
		char *head_ptr = &line[0];
		size_t i = strtol(head_ptr, &head_ptr, base10);
		size_t j = strtol(head_ptr, &head_ptr, base10);
		double v = strtod(head_ptr, &head_ptr);
		return rate_t<val_type>(i-1, j-1, (val_type)v);
	} else {
		fprintf(stderr, "Error: no more entry to iterate !!\n");
		return rate_t<val_type>(0,0,0);
	}
}
/* Deprecated Implementation
template<typename val_type>
rate_t<val_type> file_iterator_t<val_type>::next() {
	int i = 1, j = 1;
	val_type v = 0;
	if (nnz > 0) {
#ifdef _USE_FLOAT_
		if(fscanf(fp, "%d %d %f", &i, &j, &v)!=3)
			fprintf(stderr, "Error: reading smat_t\n");
#else
		if(fscanf(fp, "%d %d %lf", &i, &j, &v)!=3)
			fprintf(stderr, "Error: reading smat_t\n");
#endif
		--nnz;
	} else {
		fprintf(stderr,"Error: no more entry to iterate !!\n");
	}
	return rate_t<val_type>(i-1,j-1,v);
}
*/

template<typename val_type>
smat_iterator_t<val_type>::smat_iterator_t(const smat_t<val_type>& M, int major) {
	nnz = M.nnz;
	col_idx = (major == ROWMAJOR)? M.col_idx: M.row_idx;
	row_ptr = (major == ROWMAJOR)? M.row_ptr: M.col_ptr;
	val_t = (major == ROWMAJOR)? M.val_t: M.val;
	rows = (major==ROWMAJOR)? M.rows: M.cols;
	cols = (major==ROWMAJOR)? M.cols: M.rows;
	cur_idx = cur_row = 0;
}

template<typename val_type>
rate_t<val_type> smat_iterator_t<val_type>::next() {
	while (cur_idx >= row_ptr[cur_row+1])
		cur_row++;
	if (nnz > 0)
		nnz--;
	else
		fprintf(stderr,"Error: no more entry to iterate !!\n");
	rate_t<val_type> ret(cur_row, col_idx[cur_idx], val_t[cur_idx]);
	cur_idx++;
	return ret;
}

template<typename val_type>
smat_subset_iterator_t<val_type>::smat_subset_iterator_t(const smat_t<val_type>& M, const unsigned *subset, size_t size, bool remapping_, int major_) {
	major = major_; remapping = remapping_;
	col_idx = (major == ROWMAJOR)? M.col_idx: M.row_idx;
	row_ptr = (major == ROWMAJOR)? M.row_ptr: M.col_ptr;
	val_t = (major == ROWMAJOR)? M.val_t: M.val;
	rows = (major==ROWMAJOR)? (remapping?size:M.rows): (remapping?size:M.cols);
	cols = (major==ROWMAJOR)? M.cols: M.rows;

	this->subset.resize(size);
	nnz = 0;
	for(size_t i = 0; i < size; i++) {
		unsigned idx = subset[i];
		this->subset[i] = idx;
		nnz += (major == ROWMAJOR)? M.nnz_of_row(idx): M.nnz_of_col(idx);
	}
	sort(this->subset.begin(), this->subset.end());
	cur_row = 0;
	cur_idx = row_ptr[this->subset[cur_row]];
}

template<typename val_type>
rate_t<val_type> smat_subset_iterator_t<val_type>::next() {
	while (cur_idx >= row_ptr[subset[cur_row]+1]) {
		cur_row++;
		cur_idx = row_ptr[subset[cur_row]];
	}
	if (nnz > 0)
		nnz--;
	else
		fprintf(stderr,"Error: no more entry to iterate !!\n");
	rate_t<val_type> ret_rowwise(remapping?cur_row:subset[cur_row], col_idx[cur_idx], val_t[cur_idx]);
	rate_t<val_type> ret_colwise(col_idx[cur_idx], remapping?cur_row:subset[cur_row], val_t[cur_idx]);
	//printf("%d %d\n", cur_row, col_idx[cur_idx]);
	cur_idx++;
	return major==ROWMAJOR? ret_rowwise: ret_colwise;
}


/*
   H = X*W
   X is an m*n
   W is an n*k, row-majored array
   H is an m*k, row-majored array
   */
template<typename val_type>
void smat_x_dmat(const smat_t<val_type> &X, const val_type* W, const size_t k, val_type *H)
{
	size_t m = X.rows;
//#pragma omp parallel for schedule(dynamic,50) shared(X,H,W)
	for(size_t i = 0; i < m; i++) {
		val_type *Hi = &H[k*i];
		memset(Hi,0,sizeof(val_type)*k);
		for(size_t idx = X.row_ptr[i]; idx < X.row_ptr[i+1]; idx++) {
			const val_type Xij = X.val_t[idx];
			const val_type *Wj = &W[X.col_idx[idx]*k];
			for(unsigned t = 0; t < k; t++)
				Hi[t] += Xij*Wj[t];
		}
	}
}

/*
   H = a*X*W + H0
   X is an m*n
   W is an n*k, row-majored array
   H is an m*k, row-majored array
   */
template<typename val_type>
void smat_x_dmat(const val_type a, const smat_t<val_type> &X, const val_type* W, const size_t k, const val_type *H0, val_type *H)
{
	size_t m = X.rows;
//#pragma omp parallel for schedule(dynamic,50) shared(X,H,W)
	for(size_t i = 0; i < m; i++) {
		val_type *Hi = &H[k*i];
		if(H != H0)
			memcpy(Hi, &H0[k*i], sizeof(val_type)*k);
		for(size_t idx = X.row_ptr[i]; idx < X.row_ptr[i+1]; idx++) {
			const val_type Xij = X.val_t[idx];
			const val_type *Wj = &W[X.col_idx[idx]*k];
			for(unsigned t = 0; t < k; t++)
				Hi[t] += a*Xij*Wj[t];
		}
	}
}

#undef smat_t
#endif // SPARSE_MATRIX_H
