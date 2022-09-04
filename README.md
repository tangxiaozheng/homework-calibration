# homework-calibration
#校正
	#statRRF:输出一个RRF值，用该RRF值与未测样品信号计算浓度。
		#all:将所有RRF值相加取平均值
		#close：将离未知样品曲线最近的两个浓度的已知样品的RRF值相加取平均
		#closest:将离未知样品曲线最近的一个浓度的已知样品的RRF值相加取平均
	#DynRRF:输出一条曲线，按未测样品的进样时间取曲线上一点做RRf，再计算对应浓度。
		#all:对所有RRF值的平均值做时间的二次回归
		#close:对最近的两个浓度的RRF值取平均然后做二次回归 比较RRF*c-t的直线来确定close与closest
		#closest：对最近的一个浓度的RRF做一次回归
	#statCal: 对每一级别的RRF*C平均值做关于浓度的回归，以下为他们回归方程的形式。输出一条曲线，按未测样品的信号，可以读出对应的浓度
		bx：截距为0
		a+bx
		bx+cx^2:截距为0
		a+bx+cx^2
	DynCal:按未测样品进样时间在四条RRF-t的曲线上各取一点，以此系列点建立信号（RRF*c）-浓度的回归曲线，形式如下。输出一条曲线，按未测样品的信号，可以读出对应浓度。
		bx：截距为0column
		a+bx
		bx+cx^2:截距为0
		a+bx+cx^2
RRFC= M(内标)*A（分析）/*A(内标)
