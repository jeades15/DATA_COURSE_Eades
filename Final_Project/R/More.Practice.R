# Here is what my data set is
Allyear # The total combined all data

df1 # the averages of the data important for this study

# Visualizations

ggplot(Allyear, aes(x=BA, y=Fldpct)) +
  geom_point() +
  facet_wrap(~ Tm.x)


ggplot(Allyear, aes(x=Fldpct,y=BA)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Year)
