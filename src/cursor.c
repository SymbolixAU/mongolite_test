#include <mongolite.h>

SEXP R_mongo_cursor_more (SEXP ptr){
  mongoc_cursor_t *c = r2cursor(ptr);
  return ScalarLogical(mongoc_cursor_more(c));
}

SEXP R_mongo_cursor_next_bson (SEXP ptr){
  mongoc_cursor_t *c = r2cursor(ptr);
  const bson_t *b = NULL;
  if(!mongoc_cursor_next(c, &b)){
    bson_error_t err;
    if(mongoc_cursor_error (c, &err))
      stop(err.message);
    else
      return R_NilValue;
  }
  return bson2r((bson_t*) b);
}

SEXP R_mongo_cursor_next_bsonlist (SEXP ptr, SEXP n){
  mongoc_cursor_t *c = r2cursor(ptr);
  int len = asInteger(n);
  SEXP out = PROTECT(allocVector(VECSXP, len));
  const bson_t *b = NULL;
  int total = 0;
  bson_error_t err;
  while(total < len){
    if(!mongoc_cursor_next(c, &b)){
      if(mongoc_cursor_error (c, &err))
        stop(err.message);
      else
        break; //cursor exchausted: done
    } else {
      SEXP bin = PROTECT(allocVector(RAWSXP, b->len));
      memcpy(RAW(bin), bson_get_data(b), b->len);
      SET_VECTOR_ELT(out, total, bin);
      UNPROTECT(1);
      total++;
    }
  }
  if(total < len){
    SEXP out2 = PROTECT(allocVector(VECSXP, total));
    for(int i = 0; i < total; i++){
      SET_VECTOR_ELT(out2, i, VECTOR_ELT(out, i));
    }
    UNPROTECT(2);
    return out2;
  }
  UNPROTECT(1);
  return out;
}

SEXP R_mongo_cursor_next_json (SEXP ptr, SEXP n){
  mongoc_cursor_t *c = r2cursor(ptr);
  int len = asInteger(n);
  SEXP out = PROTECT(allocVector(STRSXP, len));
  const bson_t *b = NULL;
  int total = 0;
  bson_error_t err;
  while(total < len){
    if(!mongoc_cursor_next(c, &b)){
      if(mongoc_cursor_error (c, &err))
        stop(err.message);
      else
        //cursor exchausted: done
        break;
    } else {
      size_t jsonlength;
      char *str = bson_as_json ((bson_t*) b, &jsonlength);
      SET_STRING_ELT(out, total, mkCharLenCE(str, jsonlength, CE_UTF8));
      if(str) bson_free(str);
      total++;
    }
  }
  if(total < len){
    SEXP out2 = PROTECT(allocVector(STRSXP, total));
    for(int i = 0; i < total; i++){
      SET_STRING_ELT(out2, i, STRING_ELT(out, i));
    }
    UNPROTECT(2);
    return out2;
  }
  UNPROTECT(1);
  return out;
}


// need to just iterate over one BSON document

//SEXP R_mongo_cursor_find_one(SEXP ptr){
//  mongoc_cursor_t *c = r2cursor(ptr);
//  const bson_t *b = NULL;
//  SEXP value_types = PROTECT(allocVector(STRSXP, 1))
//}


SEXP R_mongo_cursor_next_page(SEXP ptr, SEXP size){
  mongoc_cursor_t *c = r2cursor(ptr);
  int n = asInteger(size);
  // n == pagesize
  const bson_t *b = NULL;
  SEXP list = PROTECT(allocVector(VECSXP, n));
  int total = 0;
  for(int i = 0; i < n && mongoc_cursor_next(c, &b); i++){
    // mongoc_cursor_next:
    // http://api.mongodb.org/c/current/mongoc_cursor_next.html
    // iterates the underlying cursor, setting bson to the next document

    // for each BSON document, call utils::bson2list
    SET_VECTOR_ELT(list, i, bson2list((bson_t*) b, total));
    total++;
  }

  //printf("list length: %d\n", length(list));  // length of the list is the pagesize

  //printf("size: %d\n", size);
  //printf("total: %d\n", total);

  //iterator exhausted
  if(total == 0){
    UNPROTECT(1);
    return R_NilValue;
  }

  //found a full page
  if(total == n){
    UNPROTECT(1);
    return list;
  }

  //not a full page
  SEXP shortlist = PROTECT(allocVector(VECSXP, total));
  for(int i = 0; i < total; i++){
    SET_VECTOR_ELT(shortlist, i, VECTOR_ELT(list, i));
  }
  UNPROTECT(2);

  //also check for errors
  bson_error_t err;
  if(mongoc_cursor_error (c, &err))
    stop(err.message);

  printf("short list length: %d\n", length(shortlist));

  return shortlist;
}
