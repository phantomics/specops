;;;; comber.lisp

(ql:quickload 'lquery)

(defun comb-z80-specs (table)
  ;; this is a function to comb through the z80 instruction tables at
  ;; http://z80-heaven.wikidot.com/opcode-reference-chart and glean the opcode specs
  ;; for use with the (specops) macro; it depends on the lquery macro
  (let ((data (lquery:$ (initialize table)))
        (holders (list nil)) (index 0))
    (loop :for item :across (lquery:$ data "tbody tr td a" (text))
          :do (let ((output (cl-ppcre:split "[ ,]" item)))
                (when (eq 16 index) (push nil holders)
                      (setf index 0))
                (push (cons (intern (string-upcase (first output)) "KEYWORD")
                            (loop :for i :in (rest output) :collect (intern (string-upcase i))))
                      (first holders))
                (incf index)))
    (reverse (loop :for h :in holders :collect (reverse h)))))

(defun comb-wasm-specs (table)
  ;; this is a function to comb through the z80 instruction tables at
  ;; http://z80-heaven.wikidot.com/opcode-reference-chart and glean the opcode specs
  ;; for use with the (specops) macro; it depends on the lquery macro
  (let ((data (lquery:$ (initialize table)))
        (holders (list nil)) (index 0))
    (loop :for item :across (lquery:$ data "tbody tr td" (text))
          :do (let ((output (cl-ppcre:split "[. ]" item)))
                (when (eq 16 index) (push nil holders)
                      (setf index 0))
                (push (funcall (lambda (output)
                                 (cons (intern (string-upcase (first output)) "KEYWORD")
                                       (loop :for i :in (rest output)
                                             :collect (intern (string-upcase i)))))
                               (append (if (member (first output) '("i32" "i64" "f32" "f64" "v128" "i8x16" "i16x8" "i32x4" "i64x2" "f32x4" "f64x2")
                                                   :test #'string=)
                                           (list (second output) (first output))
                                           (list (first output) (second output)))
                                       (cddr output)))
                      (first holders))
                (incf index)))
    (reverse (loop :for h :in holders :collect (reverse h)))))

(defvar bla)

(setf bla "<tbody>
<tr>
<th scope=>FD 0_</th>
<td>v128.load</td>
<td>v128.load8x8_s</td>
<td>v128.load8x8_u</td>
<td>v128.load16x4_s</td>
<td>v128.load16x4_u</td>
<td>v128.load32x2_s</td>
<td>v128.load32x2_u</td>
<td>v128.load8_splat</td>
<td>v128.load16_splat</td>
<td>v128.load32_splat</td>
<td>v128.load64_splat</td>
<td>v128.store</td>
<td>v128.const</td>
<td>i8x16.shuffle</td>
<td>i8x16.swizzle</td>
<td>i8x16.splat</td>
</tr>
<tr>
<th >FD 1_</th>
<td>i16x8.splat</td>
<td>i32x4.splat</td>
<td>i64x2.splat</td>
<td>f32x4.splat</td>
<td>f64x2.splat</td>
<td>i8x16.extract_lane_s</td>
<td>i8x16.extract_lane_u</td>
<td>i8x16.replace_lane</td>
<td>i16x8.extract_lane_s</td>
<td>i16x8.extract_lane_u</td>
<td>i16x8.replace_lane</td>
<td>i32x4.extract_lane</td>
<td>i32x4.replace_lane</td>
<td>i64x2.extract_lane</td>
<td>i64x2.replace_lane</td>
<td>f32x4.extract_lane</td>
</tr>
<tr>
<th >FD 2_</th>
<td>f32x4.replace_lane</td>
<td>f64x2.extract_lane</td>
<td>f64x2.replace_lane</td>
<td>i8x16.eq</td>
<td>i8x16.ne</td>
<td>i8x16.lt_s</td>
<td>i8x16.lt_u</td>
<td>i8x16.gt_s</td>
<td>i8x16.gt_u</td>
<td>i8x16.le_s</td>
<td>i8x16.le_u</td>
<td>i8x16.ge_s</td>
<td>i8x16.ge_u</td>
<td>i16x8.eq</td>
<td>i16x8.ne</td>
<td>i16x8.lt_s</td>
</tr>
<tr>
<th >FD 3_</th>
<td>i16x8.lt_u</td>
<td>i16x8.gt_s</td>
<td>i16x8.gt_u</td>
<td>i16x8.le_s</td>
<td>i16x8.le_u</td>
<td>i16x8.ge_s</td>
<td>i16x8.ge_u</td>
<td>i32x4.eq</td>
<td>i32x4.ne</td>
<td>i32x4.lt_s</td>
<td>i32x4.lt_u</td>
<td>i32x4.gt_s</td>
<td>i32x4.gt_u</td>
<td>i32x4.le_s</td>
<td>i32x4.le_u</td>
<td>i32x4.ge_s</td>
</tr>
<tr>
<th >FD 4_</th>
<td>i32x4.ge_u</td>
<td>f32x4.eq</td>
<td>f32x4.ne</td>
<td>f32x4.lt</td>
<td>f32x4.gt</td>
<td>f32x4.le</td>
<td>f32x4.ge</td>
<td>f64x2.eq</td>
<td>f64x2.ne</td>
<td>f64x2.lt</td>
<td>f64x2.gt</td>
<td>f64x2.le</td>
<td>f64x2.ge</td>
<td>v128.not</td>
<td>v128.and</td>
<td>v128.andnot</td>
</tr>
<tr>
<th >FD 5_</th>
<td>v128.or</td>
<td>v128.xor</td>
<td>v128.bitselect</td>
<td>v128.any_true</td>
<td>v128.load8_lane</td>
<td>v128.load16_lane</td>
<td>v128.load32_lane</td>
<td>v128.load64_lane</td>
<td>v128.store8_lane</td>
<td>v128.store16_lane</td>
<td>v128.store32_lane</td>
<td>v128.store64_lane</td>
<td>v128.load32_zero</td>
<td>v128.load64_zero</td>
<td>f32x4.demote_f64x2_zero</td>
<td>f64x2.promote_low_f32x4</td>
</tr>
<tr>
<th >FD 6_</th>
<td>i8x16.abs</td>
<td>i8x16.neg</td>
<td>i8x16.popcnt</td>
<td>i8x16.all_true</td>
<td>i8x16.bitmask</td>
<td>i8x16.narrow_i16x8_s</td>
<td>i8x16.narrow_i16x8_u</td>
<td>f32x4.ceil</td>
<td>f32x4.floor</td>
<td>f32x4.trunc</td>
<td>f32x4.nearest</td>
<td>i8x16.shl</td>
<td>i8x16.shr_s</td>
<td>i8x16.shr_u</td>
<td>i8x16.add</td>
<td>i8x16.add_sat_s</td>
</tr>
<tr>
<th >FD 7_</th>
<td>i8x16.add_sat_u</td>
<td>i8x16.sub</td>
<td>i8x16.sub_sat_s</td>
<td>i8x16.sub_sat_u</td>
<td>f64x2.ceil</td>
<td>f64x2.floor</td>
<td>i8x16.min_s</td>
<td>i8x16.min_u</td>
<td>i8x16.max_s</td>
<td>i8x16.max_u</td>
<td>f64x2.trunc</td>
<td>i8x16.avgr_u</td>
<td>i16x8.extadd_pairwise_i8x16_s</td>
<td>i16x8.extadd_pairwise_i8x16_u</td>
<td>i32x4.extadd_pairwise_i16x8_s</td>
<td>i32x4.extadd_pairwise_i16x8_u</td>
</tr>
<tr>
<th >FD 8_</th>
<td>i16x8.abs</td>
<td>i16x8.neg</td>
<td>i16x8.q15mulr_sat_s</td>
<td>i16x8.all_true</td>
<td>i16x8.bitmask</td>
<td>i16x8.narrow_i32x4_s</td>
<td>i16x8.narrow_i32x4_u</td>
<td>i16x8.extend_low_i8x16_s</td>
<td>i16x8.extend_high_i8x16_s</td>
<td>i16x8.extend_low_i8x16_u</td>
<td>i16x8.extend_high_i8x16_u</td>
<td>i16x8.shl</td>
<td>i16x8.shr_s</td>
<td>i16x8.shr_u</td>
<td>i16x8.add</td>
<td>i16x8.add_sat_s</td>
</tr>
<tr>
<th >FD 9_</th>
<td>i16x8.add_sat_u</td>
<td>i16x8.sub</td>
<td>i16x8.sub_sat_s</td>
<td>i16x8.sub_sat_u</td>
<td>f64x2.nearest</td>
<td>i16x8.mul</td>
<td>i16x8.min_s</td>
<td>i16x8.min_u</td>
<td>i16x8.max_s</td>
<td>i16x8.max_u</td>
<td>&nbsp;</td>
<td>i16x8.avgr_u</td>
<td>i16x8.extmul_low_i8x16_s</td>
<td>i16x8.extmul_high_i8x16_s</td>
<td>i16x8.extmul_low_i8x16_u</td>
<td>i16x8.extmul_high_i8x16_u</td>
</tr>
<tr>
<th >FD A_</th>
<td>i32x4.abs</td>
<td>i32x4.neg</td>
<td>*i8x16.relaxed_swizzle</td>
<td>i32x4.all_true</td>
<td>i32x4.bitmask</td>
<td>*i32x4.relaxed_trunc_f32x4_s</td>
<td>*i32x4.relaxed_trunc_f32x4_u</td>
<td>i32x4.extend_low_i16x8_s</td>
<td>i32x4.extend_high_i16x8_s</td>
<td>i32x4.extend_low_i16x8_u</td>
<td>i32x4.extend_high_i16x8_u</td>
<td>i32x4.shl</td>
<td>i32x4.shr_s</td>
<td>i32x4.shr_u</td>
<td>i32x4.add</td>
<td>*f32x4.relaxed_madd</td>
</tr>
<tr>
<th >FD B_</th>
<td>*f32x4.relaxed_nmadd</td>
<td>i32x4.sub</td>
<td>*i8x16.relaxed_laneselect</td>
<td>*i16x8.relaxed_laneselect</td>
<td>*f32x4.relaxed_min</td>
<td>i32x4.mul</td>
<td>i32x4.min_s</td>
<td>i32x4.min_u</td>
<td>i32x4.max_s</td>
<td>i32x4.max_u</td>
<td>i32x4.dot_i16x8_s</td>
<td>&nbsp;</td>
<td>i32x4.extmul_low_i16x8_s</td>
<td>i32x4.extmul_high_i16x8_s</td>
<td>i32x4.extmul_low_i16x8_u</td>
<td>i32x4.extmul_high_i16x8_u</td>
</tr>
<tr>
<th >FD C_</th>
<td>i64x2.abs</td>
<td>i64x2.neg</td>
<td>&nbsp;</td>
<td>i64x2.all_true</td>
<td>i64x2.bitmask</td>
<td>*i32x4.relaxed_trunc_f64x2_s_zero</td>
<td>*i32x4.relaxed_trunc_f64x2_u_zero</td>
<td>i64x2.extend_low_i32x4_s</td>
<td>i64x2.extend_high_i32x4_s</td>
<td>i64x2.extend_low_i32x4_u</td>
<td>i64x2.extend_high_i32x4_u</td>
<td>i64x2.shl</td>
<td>i64x2.shr_s</td>
<td>i64x2.shr_u</td>
<td>i64x2.add</td>
<td>*f64x2.relaxed_madd</td>
</tr>
<tr>
<th >FD D_</th>
<td>*f64x2.relaxed_nmadd</td>
<td>i64x2.sub</td>
<td>*i32x4.relaxed_laneselect</td>
<td>*i64x2.relaxed_laneselect</td>
<td>*f64x2.relaxed_min</td>
<td>i64x2.mul</td>
<td>i64x2.eq</td>
<td>i64x2.ne</td>
<td>i64x2.lt_s</td>
<td>i64x2.gt_s</td>
<td>i64x2.le_s</td>
<td>i64x2.ge_s</td>
<td>i64x2.extmul_low_i32x4_s</td>
<td>i64x2.extmul_high_i32x4_s</td>
<td>i64x2.extmul_low_i32x4_u</td>
<td>i64x2.extmul_high_i32x4_u</td>
</tr>
<tr>
<th >FD E_</th>
<td>f32x4.abs</td>
<td>f32x4.neg</td>
<td>*f32x4.relaxed_max</td>
<td>f32x4.sqrt</td>
<td>f32x4.add</td>
<td>f32x4.sub</td>
<td>f32x4.mul</td>
<td>f32x4.div</td>
<td>f32x4.min</td>
<td>f32x4.max</td>
<td>f32x4.pmin</td>
<td>f32x4.pmax</td>
<td>f64x2.abs</td>
<td>f64x2.neg</td>
<td>*f64x2.relaxed_max</td>
<td>f64x2.sqrt</td>
</tr>
<tr>
<th >FD F_</th>
<td>f64x2.add</td>
<td>f64x2.sub</td>
<td>f64x2.mul</td>
<td>f64x2.div</td>
<td>f64x2.min</td>
<td>f64x2.max</td>
<td>f64x2.pmin</td>
<td>f64x2.pmax</td>
<td>i32x4.trunc_sat_f32x4_s</td>
<td>i32x4.trunc_sat_f32x4_u</td>
<td>f32x4.convert_i32x4_s</td>
<td>f32x4.convert_i32x4_u</td>
<td>i32x4.trunc_sat_f64x2_s_zero</td>
<td>i32x4.trunc_sat_f64x2_u_zero</td>
<td>f64x2.convert_low_i32x4_s</td>
<td>f64x2.convert_low_i32x4_u</td>
</tr>
</tbody>")
