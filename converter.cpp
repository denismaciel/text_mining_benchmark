// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h>
#include <string.h>
#include <ctime>
using namespace Rcpp;

NumericMatrix get_average_vectors(List x,NumericMatrix wc,CharacterVector words,CharacterVector stopwords)
{
arma::mat word_vectors=as<arma::mat>(as<NumericMatrix>(wc));  
//Rcout<<word_vectors(0,0);
arma::mat results;
long long size3=word_vectors.n_cols;
long long size=x.size();  
//Rcout<<size;  
results.set_size(size,word_vectors.n_cols);
results.fill(0);

for(long long i=0;i<size;i++)
{
  Rcout<<"Row "<<i<<std::endl;
  CharacterVector d=x[i];
  long long size2=d.size();
  long long count=0;  
  for(long long j=0;j<size2;j++)
  {
   
    bool flag=true;
    long long stop_size=stopwords.size();
    for(long long k=0;k<stop_size;k++)
    { 
      if(d[j]==stopwords[k])
      {
        flag=false;
      }
    }
    if(flag)
    {
      long long word_size=words.size();
      for(long long k=0;k<word_size;k++)
      {
        if(d[j]==words[k])
        {
           count++;
     
           for(long long l=0;l<size3;l++)
           {
            results(i,l)+=word_vectors(k,l);
           }
        }
      }
    }
  }
  for(long long l=0;l<size3;l++)
           {
            if(count!=0)
            {
              results(i,l)/=count;
            }
           }
}
return wrap(results);
} 


NumericMatrix get_bag_of_centroids(List x,CharacterVector words,IntegerVector clusters,int num_clusters)
{
  long long size=x.size();
arma::mat results;
results.set_size(size,num_clusters);
results.fill(0);

for(long long i=0;i<size;i++)
{
  Rcout<<"Row "<<i<<std::endl;
  CharacterVector d = x[i];
  long long size2=d.size();
  for(long long j=0;j<size2;j++)
  {
    long long word_size=words.size();
    for(long long k=0;k<word_size;k++)
    {
      if(words[k]==d[j])
      {
        results(i,clusters(k)-1)=results(i,clusters(k)-1)+1;
        break;
      }
    }
  }
}
return wrap(results);
}

RCPP_MODULE(word2vec)
{
	function("get_average_vectors",&get_average_vectors);
  function("get_bag_of_centroids",&get_bag_of_centroids)
	;
}
