%   ds4    %  returns x,y, and z;  z is f1(x,y)
%  Franke/McMahon DS3 plus 100 random points
%    200    6   33   33    6    6100+100 CRDEN,EXP H&D,05        99887766
%     0.00000   0.03125   0.00000   0.03125   1.00000   0.00000
xyz=[
   0.00313  -0.01464   0.79913  -0.00579   0.15323   0.70680
  -0.01634   0.28101   0.66977  -0.01995   0.41237   0.61910
  -0.02010   0.63015   0.32341   0.04431   0.00596   0.70301
   0.07345   0.08268   0.91789   0.03820   0.22063   0.89356
   0.04849   0.35817   0.74699   0.02935   0.50274   0.56320
   0.07188   0.88049   0.21573   0.08280  -0.00480   0.90082
   0.10010   0.14756   0.97714   0.10580   0.30547   0.92799
   0.08327   0.32717   0.83714   0.11557   0.42887   0.67900
   0.10292   0.90452   0.24118   0.16040   0.06010   0.87255
   0.15711   0.22619   1.20620   0.14087   0.37665   0.83892
   0.14623   0.69178   0.37999   0.17655   0.11606   1.09308
   0.20680   0.22374   1.22112   0.22228   0.52317   0.62168
   0.17803   0.61525   0.35542   0.27420   0.15391   1.02426
   0.27047   0.24481   1.04098   0.23939   0.54758   0.45191
   0.24029   0.87419   0.34675   0.29820   0.12206   1.03854
   0.32450   0.17293   1.09369   0.30377   0.31851   1.00939
   0.28233   0.46249   0.58790   0.28964   0.68581   0.37034
   0.28175   0.98011   0.19283   0.35141   0.07342   0.71608
   0.35282   0.21140   0.99486   0.34338   0.23916   0.92140
   0.36061   0.33996   0.75694   0.33420   0.45980   0.54006
   0.33428   0.63186   0.28363   0.35783   0.77750   0.08327
   0.34339   0.93393   0.16163   0.34550   0.97516   0.15175
   0.39950   0.20157   0.73196   0.39491   0.31475   0.61878
   0.37644   0.38940   0.61176   0.41835   0.56591   0.33560
   0.37896   0.71889   0.22840   0.38352   0.73257   0.11741
   0.40823   1.02457   0.19137   0.45850   0.12050   0.54805
   0.44772   0.14323   0.61825   0.45108   0.37446   0.59258
   0.43091   0.44181   0.54921   0.45029   0.77262  -0.04568
   0.46085   0.85090   0.15527   0.48263   0.12913   0.51068
   0.50693   0.30025   0.51918   0.51365   0.65405   0.19721
   0.50059   0.67724   0.14660   0.48387   0.85145  -0.00379
   0.53826   0.04941   0.39068   0.54201   0.24671   0.51123
   0.56664   0.46878   0.33449   0.55872   0.59939   0.18625
   0.53053   0.78894   0.08384   0.60617  -0.01333   0.30549
   0.58524   0.05582   0.27995   0.57696   0.32058   0.56411
   0.61514   0.43722   0.45914   0.58835   0.68275   0.12143
   0.59939   0.92155   0.14894   0.66993   0.18322   0.44349
   0.63810   0.23750   0.49519   0.64001   0.53243   0.35693
   0.67615   0.05992   0.28146   0.71681   0.24378   0.60345
   0.68112   0.47101   0.44513   0.74294   0.07547   0.31972
   0.74070   0.32119   0.68771   0.74872   0.54883   0.35457
   0.72779   0.99319   0.00810   0.80535   0.51493   0.36974
   0.78907   0.63796   0.21524   0.86740   0.28300   0.60488
   0.87323   0.40725   0.33642   0.86348   0.83251   0.05356
   0.87954   0.10856   0.24734   0.90205   0.22479   0.33120
   0.91439   0.39291   0.29398   0.90399   0.81195   0.12318
   0.89389   0.82850   0.09389   0.92532   0.21232   0.33638
   0.93120   0.45006   0.34865   0.93759   0.58340   0.24104
   1.00121   0.07809   0.08833   0.99414   0.20735   0.25371
   1.02262   0.44645   0.10917   1.00314   0.60568   0.09224
   0.02270  -0.03102   0.83348   0.05399   0.15867   0.93163
   0.02170   0.25769   0.93773   0.01751   0.34140   0.64896
   0.00190   0.49436   0.55448  -0.05097   0.57829   0.40250
   0.03954   0.69934   0.31936  -0.04871   0.74702   0.26929
   0.03158   0.91076   0.27793  -0.04188   0.99629   0.32653
   0.13242   0.05013   0.95451   0.10903   0.09186   1.02427
   0.12544   0.25930   1.00522   0.09345   0.33816   0.90130
   0.07676   0.41711   0.55257   0.14519   0.56156   0.36631
   0.06265   0.65522   0.43080   0.14527   0.75241   0.24867
   0.09587   0.91465   0.24654   0.06956   0.96324   0.16949
   0.26456   0.02929   0.84690   0.23916   0.06023   0.99245
   0.20890   0.26688   1.12021   0.27673   0.36960   0.76664
   0.17147   0.48017   0.63842   0.22668   0.59406   0.33134
   0.19092   0.68788   0.32692   0.18676   0.81856   0.23143
   0.23046   0.90465   0.26947   0.24262   0.98054   0.19232
   0.36632   0.03970   0.75420   0.38577   0.06845   0.66553
   0.38324   0.23895   0.85079   0.31791   0.31241   0.96358
   0.34663   0.49030   0.39404   0.37766   0.51993   0.39442
   0.38732   0.64452   0.30344   0.38129   0.82038   0.13512
   0.37954   0.89381   0.23581   0.28035   0.97117   0.22089
   0.41498  -0.02846   0.59198   0.42777   0.15610   0.75902
   0.42000   0.22625   0.69952   0.46636   0.31751   0.52925
   0.48557   0.38914   0.38895   0.40920   0.50849   0.42327
   0.47926   0.63242   0.23019   0.48123   0.75110  -0.04131
   0.39778   0.84897   0.16839   0.40273   0.99787   0.19015
   0.58487  -0.02719   0.27716   0.57301   0.12724   0.37753
   0.60639   0.27093   0.54647   0.50139   0.34777   0.41443
   0.57413   0.42594   0.44406   0.61070   0.60847   0.21013
   0.59901   0.67338   0.23335   0.53806   0.72352   0.06663
   0.60970   0.92424   0.16503   0.50262   1.03088   0.18878
   0.66169   0.02560   0.38732   0.64278   0.07078   0.28623
   0.63965   0.20083   0.38582   0.67040   0.32598   0.57333
   0.70012   0.48907   0.38552   0.63336   0.50963   0.40570
   0.69089   0.66979   0.23734   0.68956   0.77596   0.12836
   0.67189   0.93661   0.12823   0.68377   1.00645   0.10120
   0.77369   0.02854   0.28187   0.76353   0.10214   0.28927
   0.74104   0.19366   0.49332   0.82590   0.32358   0.54055
   0.73060   0.47142   0.43942   0.80866   0.60916   0.19412
   0.82145   0.66851   0.14051   0.72906   0.80228   0.09991
   0.80766   0.84768   0.16523   0.81710   1.05124   0.11306
   0.84246   0.03805   0.24297   0.86841   0.09020   0.23209
   0.83669   0.20831   0.45964   0.94185   0.33185   0.37638
   0.84781   0.43356   0.39164   0.85996   0.59101   0.22162
   0.91757   0.63074   0.09719   0.85963   0.81448   0.09577
   0.92799   0.90423   0.09176   0.85128   0.96960   0.14506
   1.04498  -0.01209   0.13525   0.96706   0.13341   0.19250
   0.98579   0.26958   0.33090   0.96763   0.37953   0.34427
   1.01293   0.43961   0.14939   0.96570   0.50444   0.20478
   1.00199   0.69415   0.09506   1.03593   0.74599   0.05490
   1.04147   0.86821   0.06939   0.94715   0.98014   0.13660];
x=[xyz(:,1);xyz(:,4)];
y=[xyz(:,2);xyz(:,5)];
z=[xyz(:,3);xyz(:,6)];
clear xyz