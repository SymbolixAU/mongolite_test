#include <mongolite.h>

SEXP ConvertArray(bson_iter_t* iter, bson_iter_t* counter);
SEXP ConvertObject(bson_iter_t* iter, bson_iter_t* counter, int total);
SEXP ConvertValue(bson_iter_t* iter);
SEXP ConvertBinary(bson_iter_t* iter);
SEXP ConvertDate(bson_iter_t* iter);

SEXP R_json_to_bson(SEXP json){
  bson_t *b;
  bson_error_t err;

  b = bson_new_from_json ((uint8_t*)translateCharUTF8(asChar(json)), -1, &err);
  if(!b)
    stop(err.message);

  return bson2r(b);
}

SEXP R_raw_to_bson(SEXP buf){
  bson_error_t err;
  bson_t *b = bson_new_from_data(RAW(buf), length(buf));
  if(!b)
    stop(err.message);
  return bson2r(b);
}

SEXP R_bson_to_json(SEXP ptr){
  return mkStringUTF8(bson_as_json (r2bson(ptr), NULL));
}

SEXP R_bson_to_raw(SEXP ptr){
  bson_t *b = r2bson(ptr);
  const uint8_t *buf = bson_get_data(b);
  return mkRaw(buf, b->len);
}

SEXP R_bson_to_list(SEXP ptr) {
  bson_t *b = r2bson(ptr);
  return bson2list(b, -1);
}

SEXP ConvertValue(bson_iter_t* iter){
  if(BSON_ITER_HOLDS_INT32(iter)){
    return ScalarInteger(bson_iter_int32(iter));
  } else if(BSON_ITER_HOLDS_NULL(iter)){
    return R_NilValue;
  } else if(BSON_ITER_HOLDS_BOOL(iter)){
    return ScalarLogical(bson_iter_bool(iter));
  } else if(BSON_ITER_HOLDS_DOUBLE(iter)){
    return ScalarReal(bson_iter_double(iter));
  } else if(BSON_ITER_HOLDS_INT64(iter)){
    return ScalarReal((double) bson_iter_int64(iter));
  } else if(BSON_ITER_HOLDS_UTF8(iter)){
    return mkStringUTF8(bson_iter_utf8(iter, NULL));
  } else if(BSON_ITER_HOLDS_CODE(iter)){
    return mkStringUTF8(bson_iter_code(iter, NULL));
  } else if(BSON_ITER_HOLDS_BINARY(iter)){
    return ConvertBinary(iter);
  } else if(BSON_ITER_HOLDS_DATE_TIME(iter)){
    return ConvertDate(iter);
  } else if(BSON_ITER_HOLDS_OID(iter)){
    const bson_oid_t *val = bson_iter_oid(iter);
    char str[25];
    bson_oid_to_string(val, str);
    return mkString(str);
  } else if(BSON_ITER_HOLDS_ARRAY(iter)){
    bson_iter_t child1;
    bson_iter_t child2;
    bson_iter_recurse (iter, &child1);
    bson_iter_recurse (iter, &child2);
    return ConvertArray(&child1, &child2);
//  } else if(BSON_ITER_HOLDS_DOCUMENT(iter)){
//    bson_iter_t child1;
//    bson_iter_t child2;
//    bson_iter_recurse (iter, &child1);
//    bson_iter_recurse (iter, &child2);
//    return ConvertObject(&child1, &child2, -1);
  } else {
    stop("Unimplemented BSON type %d\n", bson_iter_type(iter));
  }
}

SEXP ConvertDate(bson_iter_t* iter){
  SEXP list = PROTECT(allocVector(VECSXP, 1));
  SET_VECTOR_ELT(list, 0, ScalarReal((double) bson_iter_date_time(iter)));
  setAttrib(list, R_NamesSymbol, mkString("$date"));
  UNPROTECT(1);
  return list;
}

SEXP ConvertBinary(bson_iter_t* iter){
  bson_subtype_t subtype;
  uint32_t binary_len;
  const uint8_t *binary;
  bson_iter_binary(iter, &subtype, &binary_len, &binary);

  //create raw vector
  SEXP out = PROTECT(allocVector(RAWSXP, binary_len));
  for (int i = 0; i < binary_len; i++) {
    RAW(out)[i] = binary[i];
  }
  setAttrib(out, install("subtype"), ScalarInteger(subtype));
  UNPROTECT(1);
  return out;

}

SEXP ConvertArray(bson_iter_t* iter, bson_iter_t* counter){
  SEXP ret;
  int count = 0;
  while(bson_iter_next(counter)){
    count++;
  }
  PROTECT(ret = allocVector(VECSXP, count));
  for (int i = 0; bson_iter_next(iter); i++) {
    SET_VECTOR_ELT(ret, i, ConvertValue(iter));
  }
  UNPROTECT(1);
  return ret;
}

SEXP ConvertObject(bson_iter_t* iter, bson_iter_t* counter, int total){

  const bson_value_t *value;
  bson_type_t type;

  SEXP names;
  SEXP ret;
  SEXP types;

  int count = 0;
  //  int count2 = 0;
  while(bson_iter_next(counter)){
    // counts records in individual collection
    // i.e. the number of 'columns' to return
    count++;
  }
  // checking if iter1 & iter2 are the same
  //  while(bson_iter_next(iter)){
  //    // counts records in individual collection
  //    // i.e. the number of 'columns' to return
  //    count2++;
  //  }
  //  printf("count: %d\n", count);   // count = number of records (columns) in the document
  //  printf("count2: %d\n", count2);   // count = number of records (columns) in the document

  //http://adv-r.had.co.nz/C-interface.html
  // allocVector() creates an R-level object
  PROTECT(ret = allocVector(VECSXP, count));      //VECSXP = lsit
  PROTECT(names = allocVector(STRSXP, count));    //STRSXP = character vector
  //PROTECT(types = allocVector(STRSXP, count));

  for (int i = 0; bson_iter_next(iter); i++) {
    // iterates
    SET_STRING_ELT(names, i, mkChar(bson_iter_key(iter)));

    //http://stackoverflow.com/questions/28557541/c-accessing-value-type-when-iterating-bson
    printf ("Found element key: \"%s\"\n", bson_iter_key (iter));
    type = bson_iter_type (iter);
    //printf("type %d\n", (int)type);  // also prints out the type
//    value = bson_iter_value (iter);
//    printf("value of type %d\n", value -> value_type);

    printf("address: %p\n", (void*)&iter);
    value = bson_iter_value (iter);

    SET_VECTOR_ELT(ret, i, ConvertValue(iter));
    printf("i: %d\n", i);


    //if i == 1, record types
    if ( total == 0 ) { // the first document
      value = bson_iter_value (iter);
      printf("value type: %d\n", value -> value_type);
    }
      //SET_VECTOR_ELT(types, i, value_type)

    // TO DO?
    // store the type of the first iter
    // move the pointer to an array
    // if a bson document doesn't have a value, set it to NULL??
    // add to the array for each iter
    // return the array
    // outsie of this loop, convert the columns of the array to vectors?/a representaion of an r data.table?
    // cast the whole column/vector in one go, based on the type stored from the first iter.

    // step 1
    // - at i == 1, create a vector of the types of each data element.

  }

  //  printf("ret.length: %d\n", length(ret));   // length is the number of columns
  //  for(int x = 0; x < ret.Length; x++){
  //    values[x] = Convert.ToInt32(array[x].ToString());
  //  }

  // convert vector

  setAttrib(ret, R_NamesSymbol, names);

  UNPROTECT(2);
  return ret;
}
