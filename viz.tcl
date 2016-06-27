# vmd -e file.tcl
puts "Number of arguments $argc"
if { $argc != 1 } {
   #                                    -1      0
   puts "USAGE:   vmd -e viz.tcl -args inFile"
   puts "EXAMPLE: vmd -e viz.tcl -args system.xyz"
   exit
}
set inFile  [lindex $argv 0]

package require pbctools
package require topotools

mol new $inFile autobonds no waitfor all

set sel [atomselect top all]
$sel set radius 0.5
$sel delete
unset sel

set cell [pbc set {5.0 10.0 0.5} -all]

pbc box 
display resetview
display projection orthographic
#rotate x by -90
#rotate y by -120
