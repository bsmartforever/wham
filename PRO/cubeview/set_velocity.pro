pro set_velocity, state
;
; This procedure sets the velocity text field of the Wham widget program


vel = state.velocity(0)+state.conditions(2)*state.velocity(1)
vel1 = string(vel)
vel2 = strcompress(vel1)
vel3 = vel2 + ' km/s'
widget_control, state.widgetids(9),	$
			set_value = vel3
return
end