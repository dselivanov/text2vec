#ifndef _PETSC_READER_H_
#define _PETSC_READER_H_

#include <vector>
#include <assert.h>
#include "sparse_matrix.h"

// ============  Input  ===============
// PETSc format

template<typename val_type>
class PETSc_reader{//{{{
	public:
		PETSc_reader(const char *src);

		void receive_rows(size_t start, size_t end, sparse_matrix<val_type> &R, std::vector<int> *col_perm = NULL);

		void clear_space();
		/* 
		void receive_rows(int procid, sparse_matrix<val_type> &R, vector<int> *col_perm = NULL) {
			// row indices to load from the file
			int start = block_row_ptr[procid];
			int end = block_row_ptr[procid+1];
			receive_rows(start, end, R, col_perm);
		}
		*/
		const char *filesrc;
		size_t rows, cols;
		size_t nnz;  // nnz of the entire data, instead of the nnz of the local data
		size_t headersize;
		std::vector<int32_t> nnz_row;
		std::vector<size_t> row_ptr;

}; //}}}



template<typename val_type>
PETSc_reader<val_type>::PETSc_reader(const char *src): filesrc(src) {
	const int UNSIGNED_FILE = 1211216, LONG_FILE = 1015;
	FILE *fp = fopen(src,"r");
	assert(fp!=NULL);

	int32_t int_buf[3];
	headersize = 0;
	headersize += sizeof(int)*fread(int_buf, sizeof(int), 3, fp);
	int filetype = int_buf[0];
	rows = (size_t) int_buf[1];
	cols = (size_t) int_buf[2];
	if(filetype == UNSIGNED_FILE) {
		headersize += sizeof(int)*fread(int_buf, sizeof(int32_t), 1, fp);
		nnz = (size_t) int_buf[0];
	} else if(filetype == LONG_FILE) {
		headersize += sizeof(int64_t)*fread(&nnz, sizeof(int64_t), 1, fp);
	} else {
		fprintf(stderr,"Wrong file type\n!");
	}
	nnz_row.resize(rows);
	row_ptr.resize(rows+1);
	headersize += sizeof(int32_t)*fread(&nnz_row[0], sizeof(int32_t), rows, fp);
	row_ptr[0] = 0;
	for(size_t i = 1; i <= rows; i++)
		row_ptr[i] = row_ptr[i-1] + nnz_row[i-1];
	fclose(fp);
}

template<typename val_type>
void PETSc_reader<val_type>::clear_space() {
	nnz_row.resize(0);
	row_ptr.resize(0);
	std::vector<int32_t>(nnz_row).swap(nnz_row);
	std::vector<size_t>(row_ptr).swap(row_ptr);
}

template<typename val_type>
void PETSc_reader<val_type>::receive_rows(size_t start, size_t end, sparse_matrix<val_type> &R, std::vector<int> *col_perm) {
	R.rows = rows;
	R.cols = cols;

	// Read CSR from the binary PETSc format
	R.nnz = row_ptr[end] - row_ptr[start];
	R.val_t = MALLOC(val_type, R.nnz);
	R.col_idx = MALLOC(unsigned, R.nnz);
	R.row_ptr = MALLOC(size_t, R.rows+1);

	FILE *fp = fopen(filesrc,"r");

	// Read R.col_idx
	fseek(fp, headersize+row_ptr[start]*sizeof(int), SEEK_SET);
	fread(&R.col_idx[0], sizeof(int), R.nnz, fp);

	// Read R.val_t
	{
		fseek(fp, headersize+nnz*sizeof(int)+row_ptr[start]*sizeof(double), SEEK_SET);
		const size_t chunksize = 10240;
		double buf[chunksize];
		size_t idx = 0;
		while(idx + chunksize < R.nnz) {
			fread(&buf[0], sizeof(double), chunksize, fp);
			for(size_t i = 0; i < chunksize; i++)
				R.val_t[idx+i] = (val_type) buf[i];
			idx += chunksize;
		}
		size_t remaining = R.nnz - idx;
		fread(&buf[0], sizeof(double), remaining, fp);
		for(size_t i = 0; i < remaining; i++)
			R.val_t[idx+i] = (val_type) buf[i];
	}
	fclose(fp);

	// Construct row_ptr
	{
		memset(R.row_ptr, 0, sizeof(size_t)*(R.rows+1));
		if(col_perm != NULL) {
			for(size_t idx = 0; idx < R.nnz; idx++)
				R.col_idx[idx] = col_perm->at(R.col_idx[idx]);
		}
		for(size_t r = start; r <= end; r++)
			R.row_ptr[r] = row_ptr[r] - row_ptr[start];
		for(size_t r = end+1; r <= rows; r++)
			R.row_ptr[r] = R.row_ptr[end];
	}

	// Convert CSR to CSC, same as csr_to_csc() in sparse_matrix.h
	{ 
		R.val = MALLOC(val_type,R.nnz);
		R.row_idx = MALLOC(unsigned,R.nnz);
		R.col_ptr = MALLOC(size_t,R.cols+1);
		size_t *col_ptr = R.col_ptr; 
		memset(col_ptr, 0, sizeof(size_t)*(R.cols+1));
		for(size_t idx = 0; idx < R.nnz; idx++)
			col_ptr[R.col_idx[idx]+1]++;
		for(size_t c = 1; c <= R.cols; c++)
			col_ptr[c] += col_ptr[c-1];
		for(size_t r = start; r < end; r++){
			for(size_t idx = R.row_ptr[r]; idx != R.row_ptr[r+1]; idx++){
				size_t c = (size_t)R.col_idx[idx];
				R.row_idx[col_ptr[c]] = r;
				R.val[col_ptr[c]++] = R.val_t[idx];
			}
		}
		for(size_t c = R.cols; c > 0; c--)
			col_ptr[c] = col_ptr[c-1];
		col_ptr[0] = 0;
	}

	R.from_mpi();
}


#endif
