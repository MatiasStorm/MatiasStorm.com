from django.urls import include, path, re_path
from rest_framework import routers
from . import views


router = routers.DefaultRouter(trailing_slash=False)
router.register(r'post_category', views.PostCategoryViewSet, basename="post-category")
router.register(r'serie', views.SerieViewSet, basename="serie")
router.register(r'post', views.PostViewSet, basename="post")
router.register(r'stripped_post', views.StrippedPostViewSet, basename="stripped-post")

app_name = "blog"

urlpatterns = [
    path('', include(router.urls)),
]
