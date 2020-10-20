from django.urls import include, path
from rest_framework import routers
from . import views


router = routers.DefaultRouter()
router.register(r'post_category', views.PostCategoryViewSet)
router.register(r'serie', views.SerieViewSet)
router.register(r'post', views.PostViewSet)

urlpatterns = [
    path('', include(router.urls))
]
