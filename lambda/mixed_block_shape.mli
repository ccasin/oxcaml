(******************************************************************************
 *                             flambda-backend                                *
 *                        Xavier Clerc, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

type 'a t

val of_mixed_block_elements : 'a Lambda.mixed_block_element array -> 'a t

val reorder_array : 'a t -> 'b array -> 'b array

val get : 'a t -> int -> 'a Lambda.mixed_block_element

val prefix : 'a t -> 'a Lambda.mixed_block_element array

val suffix : 'a t -> 'a Lambda.mixed_block_element array

val value_prefix_len : 'a t -> int

val flat_suffix_len : 'a t -> int

val original_shape : 'a t -> 'a Lambda.mixed_block_element array

val reordered_shape : 'a t -> 'a Lambda.mixed_block_element array

val reordered_shape_unit : 'a t -> unit Lambda.mixed_block_element array

val old_index_to_new_index : 'a t -> int -> int

val new_index_to_old_index : 'a t -> int -> int
