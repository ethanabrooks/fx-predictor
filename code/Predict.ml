open Types
open NBModel
open DataRecord

let _debug = false

module type NB =
  sig

    (* returns a sorted list of category, probability pairs *)
    val probabilities :
      NBModel.nb_model -> Data.record -> (Data.category*float) list

    val accuracy :
      NBModel.nb_model -> Data.record -> float

  end

module Predict : NB =
  struct
    (* The Naive Bayes equation with smoothing:
     * P(X∧C) = P(X|C)P(C) = P(C) Π (i=1..n) P(Xi|C) =
     * exp( Log P(C) + ∑ (i=1..n) Log P(Xi|C) ) *)
    let prob_x_or_c (m:NBModel.nb_model) (dr:Data.record) (c:Data.category) 
    : float =
      let log_p_c (* Log P(C) *) = m#log_cat_prior_prob c in 
      let sum_values (* (i=1..n) Log P(Xi|C) *) = m#log_cat_cond_prob dr c in

      if _debug then Printf.printf "\n
        C = %s
        Log P(C) = %f \n {Log P(Xi|C)} = %s \n"
        (Data.category_to_string c) log_p_c (concat_floats sum_values);

      exp (sum (log_p_c::sum_values))
    ;;

    type cat_prob = Data.category*float

    (* Divides a list of cat_probs into the one with the highest 
     * probability and a list of the rest *)
    let partition (init:cat_prob) (cat_probs:cat_prob list) 
    : cat_prob*cat_prob list =
      let rec partition (cat_probs:cat_prob list) (highest:cat_prob) 
      (rest:cat_prob list) : cat_prob*cat_prob list =
        match cat_probs with
        | [] -> highest, rest
        | hd::tl -> 
            let (cat1, prob1), (cat2, prob2) = hd, highest in
            if prob1 > prob2 then
              partition tl hd (highest::rest)
            else partition tl highest (hd::rest)
      in partition cat_probs init []

    (* Sorts a list of cat_probs by probability. Uses insertion sort. *)
    let sort (cat_probs:cat_prob list) =
      let rec sort (unsorted:cat_prob list) (sorted:cat_prob list) =
        match unsorted with
        | [] -> sorted
        | hd::tl ->
            let highest, rest = partition hd tl in
            sort rest (highest::sorted)
      in sort cat_probs []
    ;;

    (* Generates a list of cat_probs for each category in Data.category_list *)
    let probabilities (m:NBModel.nb_model) (dr:Data.record)
    : cat_prob list =
      let numerators (* {(j=1..n) P(X∧Cj)} *) =
        List.map (prob_x_or_c m dr) Data.category_list in
      let denominator (* P(X) *) = sum numerators in
      let unsorted = zip Data.category_list 
        (List.map (fun n -> n /. denominator) numerators) in
      assert(sum (List.map snd unsorted) =~ 1.);
      sort unsorted

    (* returns the category with the highest probability *)
    let predict (m:NBModel.nb_model) (d:Data.record) : Data.category =
      match probabilities m d with
      | [] -> failwith "predict: probabilities returned an empty list"
      | hd::tl -> let (category,_),_ = partition hd tl in category

    let accuracy (m:NBModel.nb_model) (d:Data.record) : float =
      if Data.category_to_string (predict m d) = Data.category d
      then 100. else 0.

  end
