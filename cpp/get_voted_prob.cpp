#include <Rcpp.h>
#include <vector>
#include <valarray>

using std::vector;

// [[Rcpp::export]]
Rcpp::NumericVector get_voted_prob_test(
                                Rcpp::NumericVector real_1,
                                Rcpp::NumericVector real_x,
                                Rcpp::NumericVector real_2,

                                int this_id = 1,
                                int allow_fails = 4){

    int start_id = 1;
    int end_id = 13;
    int next_id = this_id+2;

    double match1 = real_1[this_id];
    double match2 = real_1[this_id+1];

    //std::valarray<double> out_allowed_1_fails = prob[prob < allow_fails];
    //std::valarray<double> out_allowed_2_fails = prob[prob+1 < allow_fails];

    int init[]= {10,20,30,40};
    std::valarray<int> vect (init,4);

    vect = vect * 2;

    return Rcpp::NumericVector(begin(vect), end(vect));

}


/*

                                //std::valarray<std::string> sign,
                                //std::valarray<double> prob,
                                //std::valarray<double> voted,
                                //std::valarray<int> fails,


    // Normal iteration
    if(this_id != start_id){

        out_allowed_1_fails <- out[fails < allow_fails]
        out_allowed_2_fails <- out[fails+1 < allow_fails]

        out <- rbind(out[,.(sign = paste0(sign, '11'), prob = prob * match1$real_1 * match2$real_1,
                        voted  = voted * match1$voted_1 * match2$voted_1, fails)],
                 out[,.(sign = paste0(sign, 'x1'), prob = prob * match1$real_x * match2$real_1,
                        voted = voted * match1$voted_x * match2$voted_1, fails)],
                 out[,.(sign = paste0(sign, '21'), prob = prob * match1$real_2 * match2$real_1,
                        voted = voted * match1$voted_2 * match2$voted_1, fails)],
                 out[,.(sign = paste0(sign, '1x'), prob = prob * match1$real_1 * match2$real_x,
                        voted  = voted * match1$voted_1 * match2$voted_x, fails)],
                 out[,.(sign = paste0(sign, 'xx'), prob = prob * match1$real_x * match2$real_x,
                        voted = voted * match1$voted_x * match2$voted_x, fails)],
                 out[,.(sign = paste0(sign, '2x'), prob = prob * match1$real_2 * match2$real_x,
                        voted = voted * match1$voted_2 * match2$voted_x, fails)],
                 out[,.(sign = paste0(sign, '12'), prob = prob * match1$real_1 * match2$real_2,
                        voted  = voted * match1$voted_1 * match2$voted_2, fails)],
                 out[,.(sign = paste0(sign, 'x2'), prob = prob * match1$real_x * match2$real_2,
                        voted = voted * match1$voted_x * match2$voted_2, fails)],
                 out[,.(sign = paste0(sign, '22'), prob = prob * match1$real_2 * match2$real_2,
                        voted = voted * match1$voted_2 * match2$voted_2, fails)],
                 out_allowed_1_fails[,.(sign = paste0(sign, '11'), prob = prob * (match1$real_no_1 * match2$real_1 + match1$real_1 * match2$real_no_1),
                                      voted = voted * (match1$voted_no_1 * match2$voted_1 + match1$voted_1 * match2$voted_no_1), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, 'x1'), prob = prob * (match1$real_no_x * match2$real_1 + match1$real_x * match2$real_no_1),
                                      voted = voted * (match1$voted_no_x * match2$voted_1 + match1$voted_x * match2$voted_no_1), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, '21'), prob = prob * (match1$real_no_2 * match2$real_1 + match1$real_2 * match2$real_no_1),
                                      voted = voted * (match1$voted_no_2 * match2$voted_1 + match1$voted_2 * match2$voted_no_1), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, '1x'), prob = prob * (match1$real_no_1 * match2$real_x + match1$real_1 * match2$real_no_x),
                                      voted = voted * (match1$voted_no_1 * match2$voted_x + match1$voted_1 * match2$voted_no_x), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, 'xx'), prob = prob * (match1$real_no_x * match2$real_x + match1$real_x * match2$real_no_x),
                                      voted = voted * (match1$voted_no_x * match2$voted_x + match1$voted_x * match2$voted_no_x), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, '2x'), prob = prob * (match1$real_no_2 * match2$real_x + match1$real_2 * match2$real_no_x),
                                      voted = voted * (match1$voted_no_2 * match2$voted_x + match1$voted_2 * match2$voted_no_x), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, '12'), prob = prob * (match1$real_no_1 * match2$real_2 + match1$real_1 * match2$real_no_2),
                                      voted = voted * (match1$voted_no_1 * match2$voted_2 + match1$voted_1 * match2$voted_no_2), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, 'x2'), prob = prob * (match1$real_no_x * match2$real_2 + match1$real_x * match2$real_no_2),
                                      voted = voted * (match1$voted_no_x * match2$voted_2 + match1$voted_x * match2$voted_no_2), fails = fails + 1)],
                 out_allowed_1_fails[,.(sign = paste0(sign, '22'), prob = prob * (match1$real_no_2 * match2$real_2 + match1$real_2 * match2$real_no_2),
                                      voted = voted * (match1$voted_no_2 * match2$voted_2 + match1$voted_2 * match2$voted_no_2), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, '11'), prob = prob * (match1$real_no_1 * match2$real_no_1 + match1$real_no_1 * match2$real_no_1),
                                      voted = voted * (match1$voted_no_1 * match2$voted_1 + match1$voted_1 * match2$voted_no_1), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, 'x1'), prob = prob * (match1$real_no_x * match2$real_no_1 + match1$real_no_x * match2$real_no_1),
                                      voted = voted * (match1$voted_no_x * match2$voted_1 + match1$voted_x * match2$voted_no_1), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, '21'), prob = prob * (match1$real_no_2 * match2$real_no_1 + match1$real_no_2 * match2$real_no_1),
                                      voted = voted * (match1$voted_no_2 * match2$voted_1 + match1$voted_2 * match2$voted_no_1), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, '1x'), prob = prob * (match1$real_no_1 * match2$real_no_x + match1$real_no_1 * match2$real_no_x),
                                      voted = voted * (match1$voted_no_1 * match2$voted_x + match1$voted_1 * match2$voted_no_x), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, 'xx'), prob = prob * (match1$real_no_x * match2$real_no_x + match1$real_no_x * match2$real_no_x),
                                      voted = voted * (match1$voted_no_x * match2$voted_x + match1$voted_x * match2$voted_no_x), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, '2x'), prob = prob * (match1$real_no_2 * match2$real_no_x + match1$real_no_2 * match2$real_no_x),
                                      voted = voted * (match1$voted_no_2 * match2$voted_x + match1$voted_2 * match2$voted_no_x), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, '12'), prob = prob * (match1$real_no_1 * match2$real_no_2 + match1$real_no_1 * match2$real_no_2),
                                      voted = voted * (match1$voted_no_1 * match2$voted_2 + match1$voted_1 * match2$voted_no_2), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, 'x2'), prob = prob * (match1$real_no_x * match2$real_no_2 + match1$real_no_x * match2$real_no_2),
                                      voted = voted * (match1$voted_no_x * match2$voted_2 + match1$voted_x * match2$voted_no_2), fails = fails + 1)],
                 out_allowed_2_fails[,.(sign = paste0(sign, '22'), prob = prob * (match1$real_no_2 * match2$real_no_2 + match1$real_no_2 * match2$real_no_2),
                                      voted = voted * (match1$voted_no_2 * match2$voted_2 + match1$voted_2 * match2$voted_no_2), fails = fails + 1)]
    )

    out <-out[,.(prob = sum(prob), voted = sum(voted)), by=.(fails, sign)]

    if(this_id != end_id){
      return <- get_voted_prob(matches, out = out, this_id = next_id, allow_fails = allow_fails)
    }else{
      return <- out
    }

    return(return)

  }

  # First iteration
  if(this_id == start_id){

    out <- rbind(data.table(sign = '11', prob = match1$real_1 * match2$real_1,
                            voted = match1$voted_1 * match2$voted_1, fails = 0),
                 data.table(sign = 'x1', prob = match1$real_x * match2$real_1,
                            voted = match1$voted_x * match2$voted_1, fails = 0),
                 data.table(sign = '21', prob = match1$real_2 * match2$real_1,
                              voted = match1$voted_2 * match2$voted_1, fails = 0),
                 data.table(sign = '1x', prob = match1$real_1 * match2$real_x,
                            voted = match1$voted_1 * match2$voted_x, fails = 0),
                 data.table(sign = 'xx', prob = match1$real_x * match2$real_x,
                            voted = match1$voted_x * match2$voted_x, fails = 0),
                 data.table(sign = '2x', prob = match1$real_2 * match2$real_x,
                            voted = match1$voted_2 * match2$voted_x, fails = 0),
                 data.table(sign = '12', prob = match1$real_1 * match2$real_2,
                            voted = match1$voted_1  * match2$voted_2, fails = 0),
                 data.table(sign = 'x2', prob = match1$real_x * match2$real_2,
                            voted = match1$voted_x * match2$voted_2, fails = 0),
                 data.table(sign = '22', prob = match1$real_2 * match2$real_2,
                            voted = match1$voted_2 * match2$voted_2, fails = 0))
    if(allow_fails > 0){
      out <- rbind(out, data.table(sign = '11', prob = match1$real_1 * match2$real_no_1 + match1$real_no_1 * match2$real_1,
                              voted = match1$voted_1 * match2$voted_no_1 + match1$voted_no_1 * match2$voted_1, fails = 1),
                   data.table(sign = 'x1', prob = match1$real_x * match2$real_no_1 + match1$real_no_x * match2$real_1,
                              voted = match1$voted_x * match2$voted_no_1 + match1$voted_no_x * match2$voted_1, fails = 1),
                   data.table(sign = '21', prob = match1$real_2 * match2$real_no_1 + match1$real_no_2 * match2$real_1,
                              voted = match1$voted_2 * match2$voted_no_1 + match1$voted_no_2 * match2$voted_1, fails = 1),
                   data.table(sign = '1x', prob = match1$real_1 * match2$real_no_x + match1$real_no_1 * match2$real_x,
                              voted = match1$voted_1 * match2$voted_no_x + match1$voted_no_1 * match2$voted_x, fails = 1),
                   data.table(sign = 'xx', prob = match1$real_x * match2$real_no_x + match1$real_no_x * match2$real_x,
                              voted = match1$voted_x * match2$voted_no_x + match1$voted_no_x * match2$voted_x, fails = 1),
                   data.table(sign = '2x', prob = match1$real_2 * match2$real_no_x + match1$real_no_2 * match2$real_x,
                              voted = match1$voted_2 * match2$voted_no_x + match1$voted_no_2 * match2$voted_x, fails = 1),
                   data.table(sign = '12', prob = match1$real_1 * match2$real_no_2 + match1$real_no_1 * match2$real_2,
                              voted = match1$voted_1 * match2$voted_no_2 + match1$voted_no_1 * match2$voted_2, fails = 1),
                   data.table(sign = 'x2', prob = match1$real_x * match2$real_no_2 + match1$real_no_x * match2$real_2,
                              voted = match1$voted_x * match2$voted_no_2 + match1$voted_no_x * match2$voted_2, fails = 1),
                   data.table(sign = '22', prob = match1$real_2 * match2$real_no_2 + match1$real_no_2 * match2$real_2,
                              voted = match1$voted_2 * match2$voted_no_2 + match1$voted_no_2 * match2$voted_2, fails = 1))
    }
    if(allow_fails > 1){
          out <- rbind(out,
                       data.table(sign = '11', prob = match1$real_no_1 * match2$real_no_1,
                                  voted = match1$voted_no_1 * match2$voted_no_1, fails = 2),
                       data.table(sign = 'x1', prob = match1$real_no_x * match2$real_no_1,
                                  voted = match1$voted_no_x * match2$voted_no_1, fails = 2),
                       data.table(sign = '21', prob = match1$real_no_2 * match2$real_no_1,
                                  voted = match1$voted_no_2 * match2$voted_no_1, fails = 2),
                       data.table(sign = '1x', prob = match1$real_no_1 * match2$real_no_x,
                                  voted = match1$voted_no_1 * match2$voted_no_x, fails = 2),
                       data.table(sign = 'xx', prob = match1$real_no_x * match2$real_no_x,
                                  voted = match1$voted_no_x * match2$voted_no_x, fails = 2),
                       data.table(sign = '2x', prob = match1$real_no_2 * match2$real_no_x,
                                  voted = match1$voted_no_2 * match2$voted_no_x, fails = 2),
                       data.table(sign = '12', prob = match1$real_no_1 * match2$real_no_2,
                                  voted = match1$voted_no_1  * match2$voted_no_1, fails = 2),
                       data.table(sign = 'x2', prob = match1$real_no_x * match2$real_no_2,
                                  voted = match1$voted_no_x * match2$voted_no_2, fails = 2),
                       data.table(sign = '22', prob = match1$real_no_2 * match2$real_no_1,
                                  voted = match1$voted_no_2 * match2$voted_no_2, fails = 2))
    }

    return(get_voted_prob(matches, out = out, this_id = next_id, allow_fails = allow_fails))









      Rcpp::IntegerVector a = DF["a"];
      Rcpp::CharacterVector b = DF["b"];
      Rcpp::DateVector c = DF["c"];
}
*/