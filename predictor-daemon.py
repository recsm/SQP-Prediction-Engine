
# DEBUG hardcode path
import sys
import os

import time
import Pyro4

from rpy2 import rinterface
from rpy2.robjects import r, globalenv

from stats import  quartiles, stdev, median_average_deviation #, quantile,

# MD5Sum of a now-famous password. Pyro uses this to sign the message
#   Need to make sure nameserver and client use this
#   or more easily just export PYRO_HMAC_KEY='2d736347ff7487d559d7fb3cfc1e92dd'
Pyro4.config.HMAC_KEY = "2d736347ff7487d559d7fb3cfc1e92dd"

DEBUG = True

def fmt(num, digits=3):
    "Format (vector of) floats as strings, using the specified precision."
    if type(num) == float:
        return "{:1.{digits}f}".format(num, digits=digits)
    return ["{:1.{digits}f}".format(n, digits=digits) for n in num]

class Predictor:
    """Calls the R functions in predict.R to obtain predictions given the SQP
    codes."""

    def __init__(self, digits=3):
        """Load in the predictor objects in R. This takes a few seconds."""
        self.digits = digits
        self.busy = False
        
        r.setwd(os.path.join(os.path.dirname(__file__), 'predict'))
        r['source']('predict.R')

        globalenv['digits'] = digits # Set number of digits, not really used
        globalenv['django.debug'] = DEBUG # Pass Django debug setting


    def _summarize_predictions(self, est, pre, digits=3):
        """Aggregate pre in some interesting ways and return a dict of views.
        Can also add plots via matplotlib here."""
        agg = {'point_estimate':fmt(est[0]),
            'quartiles':fmt(quartiles(pre)),
            'standard_error':fmt(stdev(pre)),
            'mad':fmt(median_average_deviation(pre)),
        }
        agg['median'] = agg['quartiles'].pop(1) # Middle of quartiles is median
        return agg

    def get_predictions(self, country, language, codes, digits=3):
        """Obtain predictions from the R environment given the country, language and
        codes provided."""
        
        pr = self._get_raw_predictions(country, language, codes)

        return {\
        'reliability_coefficient' : self._summarize_predictions(pr['est.rel'], pr['pre.rel']),
        'reliability' : self._summarize_predictions(pr['est.rel2'], pr['pre.rel2']),
        'validity_coefficient' : self._summarize_predictions(pr['est.val'], pr['pre.val']),
        'validity' : self._summarize_predictions(pr['est.val2'], pr['pre.val2']),
        'quality_coefficient' : self._summarize_predictions(pr['est.qual'], pr['pre.qual']),
        'quality' : self._summarize_predictions(pr['est.qual2'], pr['pre.qual2']),
        'method_effect' : self._summarize_predictions(pr['est.met'], pr['pre.met']),
        'common_method_variance' : self._summarize_predictions(pr['est.cmv'], pr['pre.cmv']),
        }

    def _get_raw_predictions(self, country, language, codes):
        """Obtain predictions from the R environment given the country, language and
        codes provided."""
        # Signal busy in case client wants to try another server
        #   (Only worth it when the load is very high)
        self.busy = True

        # Convert the SQP codes object to objects usable by
        choices, var_names = self._get_choices(country, language, codes)

        # Obtain the predictions, getting rid of rpy2 classes
        result = globalenv['prophesize'](choices, var_names)

        # values are lists (possibly of length 1) of the predicted values.
        predictions = dict(zip(r['names'](result), result))

        self.busy = False
        return predictions

    def _get_choices(self, country, language, codes):
        """Take country, language and codes and return list of choices and names, usable
        by R. Also add missing information such as the country and language."""

        # Put variable names and choices in lists to copy to R
        choices = [code['code'] for code in codes]
        var_names = [code['characteristic_short_name'] for code in codes]

        # Add information on country and language (these are MetaVariables but
        #   not Characteristics
        # (Language is not really needed currently)
        choices.extend([country, language])
        var_names.extend(['country', 'language'])
        return choices, var_names

    def get_conditional_effects(self, country, language, codes, what, xname):
        """the conditional effects that calls the R code using
        the names in the R script. An exception may be thrown here if the
        Codings contain some value that was not in the original dataset. In
        that case no prediction can be made."""
        
        if DEBUG: start = time.time()
        # Convert the SQP codes object to objects usable by R
        choices, var_names = self._get_choices(country, language, codes)
#print globalenv.keys()
#('conditional.cmv', 'conditional.qual', 'conditional.qual2', 
#'conditional.rel', 'conditional.rel2', 'conditional.val',
# 'conditional.val2', 'digits', 'django.debug',
# 'get.conditional.effects', 'get.iqr',
# 'get.nd.from.choices', 'get.pred.interval', 
#'get.sd', 'getImp', 'invlogit', 'prophesize', 'raimforest.rel', 
#'raimforest.val', 'recode', 'rf.rel', 'rf.val', 'squeezeBlanks', 'xlevels')

        result = globalenv["conditional." + what](xname, choices, var_names)
        
        # Don't dict because they might be ordered
        predictions = zip(r['names'](result), (fmt(rs) for rs in result))

        if DEBUG:
            elapsed = time.time() - start#DEBUG
            print "Got conditional of {} effects for {}, took {:2.3f}s".format(what,
                    xname, elapsed)
            print predictions
        return predictions
    
    """
    def get_all_conditional_effects(self, country, language, codes, what):
        "" "Method to obtain all the conditional effects that calls the R code using
        the names in the R script. An exception may be thrown here if the
        Codings contain some value that was not in the original dataset. In
        that case no prediction can be made."" "
        # Signal busy in case client wants to try another server
        #   (Only worth it when the load is very high)
        self.busy = True

        xnames = list(r['names'](globalenv['xlevels']))

        res = {}
        for xname in xnames:
            res[xname] = self.get_conditional_effects(country, language,codes,what,xname)

        self.busy = False
        return res
    """
    
    def get_xlevels(self, scale_basic='0'):
        """Give xlevels' names"""
        if scale_basic == '2':
            return list(r['names'](globalenv['xlevels.fre']))
        return list(r['names'](globalenv['xlevels']))


def main():
    
    try:
        predictor_name = os.environ['PREDICTOR_NAME']
    except:
        predictor_name = 'predictor'

    
    predictor = Predictor()
    Pyro4.Daemon.serveSimple(
            {
                predictor: predictor_name,
            },
            ns=True)

if __name__=="__main__":
    main()
