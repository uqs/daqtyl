/* there's tons of magic numbers here because 
 * i exported a semi-done parametric version from clojure
 * and didn't fix all the formulas for x and y translates....
 * before getting it fully working and for getting how the working scad version
 * diverged from the clojure version.......................
 *
 * sorry future person trying to change this, you will need 
 * a strong dragon spear to modify any 'kinda adjustable' values */

reset_holder = true;
vertical = true;
cutout_blank = false;
show_previous = false;

controller_adj = 0.0;

usb_holder_x = 30.6;       // kinda adjustable
usb_holder_y = 38.8 + controller_adj;       // kinda adjustable
usb_holder_z = reset_holder? 15 : 8.4;        // kinda adjustable

usb_holder_border = 1.5;   // kinda adjustable

/* this combination of controller and usb_c dimensions 
 * friction fits both generations of elite-c controllers */
usb_elite_c_x = 18.7;      // kinda adjustable
usb_elite_c_y = 33.4 + controller_adj;      // mostly adjustable
usb_elite_c_side_cut = 6;  // adjustable

usb_c_x = 9.3;             // adjustable
usb_c_z = 4.5;             // kinda adjustable

trrs_x = 6.2;              // mostly adjustable
trrs_y = 12.7;             // kinda adjustable
trrs_r = 3.25;             // adjustable

usb_holder_center_x = usb_holder_x / 2;
usb_holder_center_y = usb_holder_y / 2;
usb_holder_center_z = usb_holder_z / 2;

usb_holder_notch_xy = usb_holder_border;
usb_holder_notch_down = usb_holder_border * usb_holder_notch_xy;
usb_holder_notch_half = usb_holder_notch_xy / 2;

$fn = 100;

module basicShape() {
  difference () {
    //basic starting shape to cut away from  `
    cube ([usb_holder_x, usb_holder_y, usb_holder_z], center=true);
    
    left_cut_x = 2 * usb_holder_border;
    left_cut_y = 3 * usb_holder_border;
    
    bottom_cut_x = (left_cut_x + trrs_x);
    bottom_cut_y = (left_cut_x + trrs_x);
    
    cut1_x = (usb_holder_center_x -  usb_holder_border);
    cut1_y = (usb_holder_center_y - (usb_holder_center_y - left_cut_y/2));
    
    cut2_x = (usb_holder_center_x - (bottom_cut_x / 2));
    //cut2_y = (usb_holder_center_y - (- usb_holder_center_y (16.6 / 2))); //todo
      
    //misc magic numbers
    if (!cutout_blank) {
      union () {
        translate ([-cut1_x, -cut1_y, 0]) {
          cube ([left_cut_x, (usb_holder_y - left_cut_y), 99], center=true);
        }
        translate ([-cut2_x, -8.3, 0]) {
          cube ([bottom_cut_x, (usb_holder_y - 16.6), 99], center=true);
        }
        rotate (a=3, v=[1, 0, 0]) {
          top_cut_adjust = reset_holder? 1.75*usb_holder_border : usb_holder_border;
          translate ([0, -(usb_holder_border * 3), (usb_holder_z / top_cut_adjust) ]) {
            cube ([usb_holder_x, usb_holder_y, usb_holder_z], center=true);
          }
        }
      }
    }
    
    if (!vertical) {
      // side notches that key into other object
      notch_x = (usb_holder_center_x - usb_holder_notch_half);
      notch_y = (usb_holder_center_y - usb_holder_notch_down);
      translate ([ notch_x, notch_y, 0]) { notch(); }
      translate ([-notch_x, notch_y, 0]) { notch(); }
    }
  }
}
module notch() {
    cube ([usb_holder_notch_xy, usb_holder_notch_xy, 30.6], center=true);
}

module trrsCutouts() {
    trrs_floor = 1;
    trrs_squareCutoutHeight = 12;
    trrs_squareCutoutHeightOffset = trrs_floor - usb_holder_center_z + trrs_squareCutoutHeight/2;
    translate ([-9.2, 11.65 + controller_adj/2, trrs_squareCutoutHeightOffset]) {
        cube ([trrs_x, trrs_y, trrs_squareCutoutHeight], center=true);
    }
    
    trrsAngle_z_adjust = reset_holder? 0.5 : 4;
    trrsAngle_z_factor = reset_holder? 0.5 : 1;
    translate ([-9.2, 11.65-trrs_y/3+ controller_adj/2, trrsAngle_z_adjust * usb_holder_border]) {
        rotate (a=-72.0, v=[1, 0, 0]) {
            cube ([trrs_x, trrs_y, usb_holder_z * trrsAngle_z_factor], center=true);
        }
    }
    
    trrs_y_offset = (usb_holder_center_y - (usb_holder_border / 2) +0.01);// + controller_adj;
    trrs_z_offset = (usb_holder_center_z - (trrs_floor + trrs_r));
    translate ([-9.1, trrs_y_offset, -trrs_z_offset]) {
        rotate (a=90.0, v=[1, 0, 0]) {
           cylinder (h=(usb_holder_border * 2), r=trrs_r, center=true);
        }
    }
}

module resetCutout() {
    reset_xz = 7.1;
    reset_y = 4.4;
    reset_floor = usb_holder_z / 1.5 ;
    reset_r = 1.85;
    
    reset_x_offset = usb_holder_center_x - usb_elite_c_x / 2 - usb_holder_border;
    reset_y_offset = usb_holder_center_y - usb_elite_c_y - usb_holder_border;
    reset_z_offset = usb_holder_center_z - (reset_floor + reset_r);
    
    translate ([reset_x_offset, -reset_y_offset, -reset_z_offset]) {
        cube ([reset_xz, reset_y, reset_xz], center=true);
    }
    
    translate ([reset_x_offset, 0, -reset_z_offset]) {
        rotate (a=90.0, v=[1, 0, 0]) {
          cylinder (h=99, r=reset_r, center=true);
        }
    }
}

module eliteC() {
    x_offset = ( (usb_holder_center_x - (usb_elite_c_x / 2)) - usb_holder_border);
    y_offset = (-(usb_holder_center_y - (usb_elite_c_y / 2)) + usb_holder_border);
    z_offset = usb_holder_border;
    translate ([x_offset, y_offset, z_offset]) {
        cube ([usb_elite_c_x, usb_elite_c_y, usb_holder_z], center=true);
    }

    left_cut_x  = ((usb_holder_center_x - (usb_elite_c_x / 2)) - ((usb_elite_c_x / 2) - (usb_elite_c_side_cut / 2)) - usb_holder_border);
    right_cut_x = ((usb_holder_center_x - (usb_elite_c_x / 2)) + ((usb_elite_c_x / 2) - (usb_elite_c_side_cut / 2)) - usb_holder_border);
    translate ([left_cut_x , y_offset, 0]) { circuitBoardSlots(); }
    translate ([right_cut_x, y_offset, 0]) { circuitBoardSlots(); }

    usbPort_z_adjust = reset_holder? -3.6 : -0.3;
    translate([0, 0, usbPort_z_adjust]) {
        usbPortCutout();
        usbRecessCutout();
    }
}

module circuitBoardSlots() {
    cube ([usb_elite_c_side_cut, usb_elite_c_y, 99], center=true);
}

module usbPortCutout() {
    usbPortCenter = ((usb_holder_center_x - (usb_elite_c_x/2)) - usb_holder_border);
    usbPortCenterCut = (usb_c_x - usb_c_z);
    usbPortSideOffset = usbPortCenterCut / 2;
    usbPortCenterCutLength = 20;
    
    translate ([(usbPortCenter - usbPortSideOffset), 20, 0]) {
        rotate (a=90.0, v=[1, 0, 0]) {
            cylinder (h=usbPortCenterCutLength, r=usb_c_z/2, center=true);
        }
    }
    
    translate ([usbPortCenter, 20, 0]) {
        cube ([usbPortCenterCut, usbPortCenterCutLength, usb_c_z], center=true);
    }
    
    translate ([(usbPortCenter + usbPortSideOffset), 20, 0]) {
        rotate (a=90.0, v=[1, 0, 0]) {
            cylinder (h=usbPortCenterCutLength, r=usb_c_z/2, center=true);
        }
    }
}

module usbRecessCutout() {
    usb_c_cover_plate = usb_holder_border;
    recess_y = (usb_holder_y - usb_elite_c_y - usb_holder_border - usb_c_cover_plate);
    recess_z = 8;
    translate ([0, (usb_holder_center_y - (recess_y / 2) +0.01), 0]) {
        
        translate ([0, 0, 0]) {
            rotate (a=90.0, v=[1, 0, 0]) {
                cylinder (h=recess_y, r=3.25, center=true);
            }
        }
        translate ([(+(usb_holder_center_x - (usb_elite_c_x/2)) - usb_holder_border), 0, 0]) {
            cube ([12.5, recess_y, recess_z], center=true);
        }
        translate ([(usb_c_x), 0, 0]) {
            rotate (a=90.0, v=[1, 0, 0]) {
                cylinder (h=recess_y, r=3.25, center=true);
            }
        }
    }
}

// main method
module usb_holder() {
    //adjust origin to front fact, bottom, mostly to match existing STLs
    translate([0, -(usb_holder_y/2), (usb_holder_z/2)]) {
        
        difference () {
            basicShape();
            if (!cutout_blank) {
                trrsCutouts();
                eliteC();
                if (reset_holder) {
                    resetCutout();
                }
            }
        }
    }
    if (show_previous) {
        color( "grey", 0.6 )
          translate([0,0,0])
            union() {
              rotate([0,0,0])
              import("../elite-c_holder_w_reset.stl");
            }
    }
}

module vertical_usb_holder() {
    difference() {
        union () {
            rotate (a=-90.0, v=[0, 1, 0]) {
                union() {
                    usb_holder();
                    translate ([0, 0, -1.5]) {
                        linear_extrude(height=usb_holder_notch_xy) {
                            projection(cut=false) {
                                usb_holder();
                            }
                        }
                    }
                }
            }
            
            // side notches that key into other object
            notch_x = (usb_holder_center_x - usb_holder_notch_half);
            notch_y = (usb_holder_center_y - usb_holder_notch_down);
            
            translate ([-15.75, -0.75, 0]) { notch(); }
            translate ([-15.75, -3.75, 0]) { notch(); }
            if (cutout_blank) {
                for ( i = [1.5:1.5:37]) {
                    translate ([-15.75, -(3.75 + i), 0]) { notch(); }
                }
            }
        }
        translate ([0.75, -usb_holder_notch_xy*1.5, 0]) { notch(); }
    }
    if (show_previous) {
        color( "grey", 0.6 )
          translate([0,0,0])
            union() {
              rotate([0,270,0])
              import("../elite-c_holder_w_reset.stl");
            }
    }
}

if (vertical) {
    vertical_usb_holder();
} else {
    usb_holder();
}
